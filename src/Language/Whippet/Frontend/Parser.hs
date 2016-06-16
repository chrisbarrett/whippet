{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
module Language.Whippet.Frontend.Parser where

import           Control.Applicative           (Alternative, (<|>))
import           Control.Lens                  hiding (op)
import           Control.Monad                 (MonadPlus)
import           Control.Monad.Trans           (MonadIO)
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Monoid                   ((<>))
import           Data.Semigroup
import           Data.String                   (fromString)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Language.Whippet.Frontend.AST hiding (functionBody)
import qualified Text.Parser.Expression        as Parser
import qualified Text.Parser.LookAhead         as Parser
import qualified Text.Parser.Token.Highlight   as Parser
import qualified Text.Parser.Token.Style       as Parser
import           Text.Trifecta                 hiding (braces, brackets, comma,
                                                eof, ident, parens, stringLit)
import qualified Text.Trifecta                 as Trifecta
import qualified Text.Trifecta.Delta           as Trifecta

-- * Custom parser

newtype P a = P {unP :: Parser a}
    deriving (
        Functor
      , Applicative
      , Monad
      , Alternative
      , MonadPlus
      , CharParsing
      , DeltaParsing
      , Parser.LookAheadParsing
      , Parsing
      , MarkParsing Trifecta.Delta
      )

instance TokenParsing P where
    someSpace = spaceParser (skipSome (oneOf " \n\r"))

spaceParser :: P () -> P ()
spaceParser p =
    Parser.buildSomeSpaceParser p comments
  where
    comments = Parser.emptyCommentStyle
                & Parser.commentLine .~ "//"

runP :: P a -> Parser a
runP = unP


-- * Parser definitions

parseFile :: MonadIO m => FilePath -> m (Result TopLevel)
parseFile =
    parseFromFileEx (runP topLevel)

parseString :: String -> Result TopLevel
parseString =
    Trifecta.parseByteString (runP topLevel) mempty . fromString

topLevel :: P TopLevel
topLevel = do
    whiteSpace
    many ast <* eof

ast :: P AST
ast =
    choice [ AstOpen <$> open <?> "open"
           , AstModule <$> module' <?> "module"
           , AstSignature <$> signature <?> "signature"
           , AstDecl <$> declaration <?> "declaration"
           ]

-- * Top-level

open :: P Open
open = do
    reserved "open"
    i <- qualifiedModule
    a <- optional (reserved "as" *> moduleName)
    h <- optional (reserved "hiding" *> parens (optional comma *> ident `sepBy` comma))
    pure (Open i a h)

signature :: P Signature
signature = do
    reserved "signature"
    i <- qualifiedModule
    b <- braces (many abstractDecl)
    pure (Signature i b)

module' :: P Module
module' = do
    reserved "module"
    i <- qualifiedModule
    b <- braces (many ast)
    pure (Module i b)

typeclass :: P Typeclass
typeclass = do
    reserved "typeclass"
    i <- typeclassName
    b <- braces (many functionOrSig)
    pure (Typeclass i b)

instance' :: P Instance
instance' = do
    reserved "instance"
    c <- qualifiedTypeclass
    t <- nominalType <|> parens typeRef
    b <- braces (many function)
    pure (Instance c t b)

abstractDecl :: P Decl
abstractDecl =
    choice [ absType, DecFunSig <$> functionSig ]
  where
    absType = do
        reserved "type"
        i <- typeName
        ps <- many typeParameter <?> "type parameter"
        pure (DecAbsType (AbsType i ps))

declaration :: P Decl
declaration =
    parser <?> "declaration"
  where
    parser =
        choice [ toDec <$> functionOrSig
               , DecRecordType <$> recordType
               , DecTypeclass <$> typeclass
               , DecInstance <$> instance'
               , decType
               ]

    toDec (Fn x)  = DecFun x
    toDec (Sig x) = DecFunSig x

    -- Concrete and abstract types share the same prefix. Delay branching in the
    -- parser to improve error messages.
    decType :: P Decl
    decType = do
        reserved "type"
        id <- typeName
        tyArgs <- many typeParameter
        concreteType id tyArgs <|> abstractType id tyArgs
      where
        concreteType id tyArgs = do
            equals
            optional pipe
            cs <- constructor `sepBy1` pipe
            pure (DecDataType (DataType id tyArgs cs))

        abstractType id tyArgs =
            pure (DecAbsType (AbsType id tyArgs))

constructor :: P Ctor
constructor =
    parser <?> "constructor"
  where
    parser = do
        i <- ctorName
        ts <- many typeRef
        pure (Ctor i ts)

function :: P Function
function = do
    reserved "let"
    i <- ident
    ps <- many fnParam <?> "parameters"
    t <- optional (try colon *> typeRef) <?> "type annotation"
    equals
    d <- functionBody
    pure (Function i ps t d)

functionBody :: P Expr
functionBody =
    choice [ parens expr
           , fnLit
           , ELit <$> stringLit
           , ELit <$> charLit
           , ELit <$> listLiteral
           , EMatch <$> match
           , EVar <$> (ident <|> ctorName)
           , hole
           , ELit <$> numberLit
           , try (ELit <$> recordLit)
           , braces expr
           ]

functionSig :: P FunctionSig
functionSig = do
    reserved "let"
    i <- ident
    colon <?> "type annotation"
    t <- typeRef
    pure (FunctionSig i t)

functionOrSig :: P FnOrSig
functionOrSig = do
    reserved "let"
    i <- ident
    ps <- many fnParam <?> "parameters"
    case ps of
      [] -> continueNoParams i
      ps -> commitFn i ps
  where
    tyAnn = optional $ do
              colon <?> "type annotation"
              typeRef

    commitFn i ps = do
        t <- tyAnn
        b <- equals *> functionBody
        pure (Fn (Function i ps t b))

    continueNoParams i = do
        t <- tyAnn
        let commitFn = Fn . Function i [] t <$> (equals *> functionBody)
            commitSig t' = pure (Sig (FunctionSig i t'))
        case t of
          Nothing -> commitFn
          Just t  -> choice [try equals *> commitFn, commitSig t]


fnParam :: P FnParam
fnParam =
    parens paramWithTy <|> paramIdent
  where
    paramWithTy = do
        i <- ident
        colon <?> "type annotation"
        t <- typeRef
        pure (FnParam i (Just t))
    paramIdent =
        FnParam <$> ident <*> pure Nothing



recordType :: P RecordType
recordType = do
    reserved "record"
    i <- typeName
    ts <- many typeParameter
    equals
    fs <- recordFields
    pure (RecordType i ts fs)


-- * Types

typeRef :: P Type
typeRef =
    Parser.buildExpressionParser operators tyTerm <?> "type"
  where
    operators = [ [Parser.Infix (pure TyApp) Parser.AssocLeft]
                , [Parser.Infix (rarrow *> pure TyArrow) Parser.AssocRight]
                ]

    tyTerm =
        choice [ parens typeRef
               , forallType
               , constraintType
               , nominalType
               , structuralType
               , typeVariable
               ]

forallType :: P Type
forallType = do
    reserved "forall"
    binders <- NonEmpty.fromList <$> some typeParameter
    dot
    t <- typeRef
    pure (TyForall binders t)

constraintType :: P Type
constraintType =
    (parens parser <|> parser) <?> "constraint"
  where
    parser =
        TyConstraint <$> try constraints <*> typeRef

    constraints = do
        res <- NonEmpty.fromList <$> constraint `sepBy1` comma
        reserved "=>"
        pure res

    constraint = do
        ctor <- typeclassName
        ps <- NonEmpty.fromList <$> some typeParameter
        pure (Constraint ctor ps)

typeVariable :: P Type
typeVariable =
    TyVar <$> ident

structuralType :: P Type
structuralType =
    TyStructural <$> recordFields

nominalType :: P Type
nominalType =
    TyNominal <$> qualifiedType

recordFields :: P [Field]
recordFields =
    braces (optional comma *> field `sepBy1` comma)
  where
    field :: P Field
    field = do
        i <- ident <?> "field name"
        colon <?> "type annotation"
        t <- typeRef
        pure (Field i t)

typeParameter :: P TypeParameter
typeParameter = do
    let style = identStyle & styleStart .~ lower
    (s :~ span) <- spanned (Trifecta.ident style)
    pure (TypeParameter (Ident span s))


-- * Expressions

expr :: P Expr
expr = do
    e <- Parser.buildExpressionParser operators term <?> "expression"
    t <- exprTypeAnnotation
    pure (maybe e (EAnnotation . Annotation e) t)
  where
    operators = [ [Parser.Infix parseApp Parser.AssocLeft]
                , [Parser.Infix parseOp Parser.AssocRight]
                ]
    parseApp =
        pure (\x y -> EApp (App x y))

    parseOp = do
        (s :~ span) <- spanned $ token (some (oneOf "<>/+-^.=!~@"))
        let op = EVar (Ident span (Text.pack s))
        pure $ \x y -> EApp (App (EApp (App op x)) y)

    term =
        choice [ parens expr
               , ELet <$> let'
               , ELit <$> recordLit
               , fnLit
               , EIf <$> ifThenElse
               , openExpr
               , ELit <$> stringLit
               , ELit <$> charLit
               , ELit <$> listLiteral
               , EMatch <$> match
               , EVar <$> (ident <|> ctorName)
               , hole
               , ELit <$> numberLit
               ]

    openExpr = do
        o <- open
        b <- expr
        pure (EOpen o b)

charLit :: P Lit
charLit = LitChar <$> charLiteral

recordLit :: P Lit
recordLit =
    LitRecord <$> braces (optional comma *> field `sepEndBy` comma)
  where
    field :: P (Ident, Expr)
    field = do
        f <- ident
        colon <?> "field value"
        e <- expr
        pure (f, e)

let' :: P Let
let' = do
    reserved "let"
    d <- discriminator
    equals
    e <- expr <* semi
    b <- expr
    pure (Let d e b)

match :: P Match
match = do
    reserved "match"
    e <- expr
    reserved "with"
    ps <- patterns
    pure (Match e ps)

exprTypeAnnotation :: P (Maybe Type)
exprTypeAnnotation = do
    c <- optional colon <?> "type annotation"
    case c of
      Nothing -> pure Nothing
      Just _ -> Just <$> tyParser <?> "type"
  where
    tyParser = parens typeRef <|> nominalType <|> structuralType <|> typeVariable


ifThenElse :: P If
ifThenElse = do
    reserved "if"
    i <- expr
    t <- reserved "then" *> expr
    e <- reserved "else" *> expr
    pure (If i t e)

fnLit :: P Expr
fnLit = do
    reserved "fn"
    EFn <$> choice [bare, patterns]
  where
    bare = do
        p <- try pat
        pure [p]

hole :: P Expr
hole = do
    let holeStyle = identStyle & styleStart .~ (letter <|> char '_')
    (s :~ span) <- spanned (Trifecta.ident holeStyle)
    pure (EHole (Ident span s))


numberLit :: P Lit
numberLit = do
    n <- integerOrScientific
    pure (either LitInt LitScientific n)

stringLit :: P Lit
stringLit =
    parser <?> "string"
  where
    parser = token $ highlight Parser.StringLiteral $ do
        str <- char '"' *> (escapeSequence <|> anyChar) `manyTill` char '"'
        pure (LitString (Text.pack str))

    escapeSequence = token $ highlight Parser.EscapeCode $ do
        ch <- char '\\' *> anyChar
        case ch of
          '\"' -> pure '\"'
          '\\' -> pure '\\'
          '/'  -> pure '/'
          'n'  -> pure '\n'
          'r'  -> pure '\r'
          'f'  -> pure '\f'
          't'  -> pure '\t'
          'b'  -> pure '\b'
          _    -> fail "Invalid escape sequence"

listLiteral :: P Lit
listLiteral =
    parser <?> "list"
  where
    parser =
      LitList <$> brackets (optional comma *> expr `sepEndBy` comma)

-- * Pattern matching

patterns :: P [Pat]
patterns = braces (optional pipe *> pat `sepBy` pipe)

pat :: P Pat
pat = do
    d <- discriminator
    e <- rarrow *> expr
    pure (Pat d e)

discriminator :: P Discriminator
discriminator = do
    e <- Parser.buildExpressionParser operators discTerm <?> "discriminator"
    t <- exprTypeAnnotation
    pure (maybe e (DAnn e) t)
  where
    discTerm = parens discriminator <|> dctor <|> dvar <|> drecord <|> dwildcard

    dctor =
        DCtor <$> ctorName
              <?> "constructor"

    dvar =
        DVar <$> ident
             <?> "pattern variable"

    drecord =
        DRec <$> braces (optional comma *> discriminator `sepBy` comma)
             <?> "record discriminator"

    dwildcard = do
        let wildcardStyle = identStyle & styleStart .~ (letter <|> char '_')
        (s :~ span) <- spanned (Trifecta.ident wildcardStyle) <?> "wildcard"
        pure (DWildcard (Ident span s))

    operators = [ [Parser.Infix (reserved "as" *> pure DAs) Parser.AssocLeft]
                , [Parser.Infix (pure DApp) Parser.AssocLeft]
                ]


-- * Helpers

reservedWords = [ "module"
                , "signature"
                , "type"
                , "record"
                , "let"
                , "if"
                , "then"
                , "else"
                , "fn"
                , "as"
                , "match"
                , "with"
                , "open"
                , "hiding"
                , "typeclass"
                , "instance"
                , "forall"
                , "="
                , "->"
                , "=>"
                , "|"
                , ";"
                ]


ctorName :: P Ident
ctorName =
    parser <?> "constructor name"
  where
    parser = do
        let style = identStyle & styleStart .~ upper
        (s :~ span) <- spanned (Trifecta.ident style)
        pure (Ident span s)

typeclassName :: P Ident
typeclassName =
    moduleName
    <?> "typeclass name"

qualifiedTypeclass :: P QualId
qualifiedTypeclass = qualifiedModule <?> "typeclass name"

qualifiedModule :: P QualId
qualifiedModule =
    parser <?> "module ID"
  where
    parser = QualId . NonEmpty.fromList <$> moduleName `sepBy1` dot


qualifiedType :: P QualId
qualifiedType =
    parser <?> "type ID"
  where
    parser = QualId . NonEmpty.fromList <$> typeName `sepBy1` dot

typeName :: P Ident
typeName = ctorName
    <?> "type name"

moduleName :: P Ident
moduleName = do
    let style =
            identStyle
               & styleStart  .~ upper
               & styleLetter .~ (alphaNum <|> oneOf "_")
    (s :~ span) <- spanned (Trifecta.ident style)
    pure (Ident span s)

-- TODO: Accept qualified var references: (modid '.')* id
ident :: P Ident
ident = do
    (s :~ span) <- spanned (Trifecta.ident identStyle)
    pure (Ident span s)

identStyle :: IdentifierStyle P
identStyle =
    Parser.emptyIdents
        & styleReserved .~ reservedWords
        & styleStart    .~ letter
        & styleLetter   .~ (alphaNum <|> oneOf "_?")

reserved :: String -> P ()
reserved s = reserve identStyle s <?> s

op :: (Monad m, TokenParsing m) => String -> m ()
op w =
    reserve ops w <?> w
  where
    ops = Parser.emptyOps & styleReserved .~ [":", "=", "|", "->"]

comma :: P ()
comma =
    op ","
    <?> "comma"

pipe :: P ()
pipe =
    op "|"
    <?> "pipe"

equals :: P ()
equals =
    op "="
    <?> "equals"

rarrow :: P ()
rarrow =
    op "->"
    <?> "right arrow"

braces :: P a -> P a
braces =
    between start end
  where
    start = op "{" <?> "open curly brace"
    end = op "}" <?> "closing brace"

parens :: P a -> P a
parens =
    between start end
  where
    start = op "(" <?> "open paren"
    end = op ")" <?> "close paren"

brackets :: P a -> P a
brackets =
    between start end
  where
    start = op "[" <?> "open square bracket"
    end = op "]" <?> "closing bracket"

eof :: P ()
eof = notFollowedBy anyChar
