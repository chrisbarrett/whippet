{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
-- |This module implements the lexing and parsing of the Whippet language. The
-- output of a successful parse is an AST annotated with source positions.
module Language.Whippet.Parser (
    -- * Parser types
      module Language.Whippet.Parser.Types
    , module Language.Whippet.Parser.Pos
    -- |Custom parser type for the language, which handles the whitespace and
    -- comment rules of Whippet.
    , P
    -- |Given a Whippet parser, extract the underlying Trifecta parser for testing.
    , runP
    -- * Parser entrypoints
    --
    -- These are the functions clients use to invoke the parser.
    , parseFile
    , parseString
    -- * Concrete parser functions
    --
    -- These are the two main language parsers. They are exported for tests.

    -- |Top-down parser for a Whippet source file.
    , topLevel
    , TopLevel
    -- |Parser for a single toplevel entry in a Whippet source file.
    , topLevelItem
    -- |Parser for expressions.
    , expr
    ) where

import           Control.Applicative           (Alternative, (<|>))
import           Control.Lens                  hiding (op)
import           Control.Monad                 (MonadPlus)
import           Control.Monad.Trans           (MonadIO)
import qualified Data.Char                     as Char
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Semigroup
import           Data.String                   (fromString)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Debug.Trace
import qualified Text.Parser.Expression        as Parser
import qualified Text.Parser.LookAhead         as Parser
import qualified Text.Parser.Token.Highlight   as Parser
import qualified Text.Parser.Token.Style       as Parser
import           Text.Trifecta                 hiding (braces, brackets, comma,
                                                eof, ident, parens, parseString,
                                                position, semi, stringLit)
import qualified Text.Trifecta                 as Trifecta
import qualified Text.Trifecta.Delta           as Trifecta

import           Language.Whippet.Parser.Pos
import           Language.Whippet.Parser.Types

-- Custom parser

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

pos :: P PosT
pos = do
    p <- Trifecta.position
    case p of
      Trifecta.Lines l c _ _ ->
          pure (Just (Pos (Line l) (Col c)))

      Trifecta.Directed _ l c _ _ ->
          pure (Just (Pos (Line l) (Col c)))

      _ ->
          pure Nothing

runP :: P a -> Parser a
runP = unP

identStyle :: IdentifierStyle P
identStyle =
    Parser.emptyIdents
        & styleReserved .~ reservedWords
        & styleStart    .~ letter
        & styleLetter   .~ (alphaNum <|> oneOf "_?")
    where
      reservedWords = [ "module"
                      , "signature"
                      , "type"
                      , "record"
                      , "let"
                      , "unless"
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


-- Parser definitions

type PosT = Maybe Pos
type TopLevel = [AST PosT]

parseFile :: MonadIO m => FilePath -> m (Result TopLevel)
parseFile =
    parseFromFileEx (runP topLevel)

parseString :: String -> Result TopLevel
parseString =
    Trifecta.parseByteString (runP topLevel) mempty . fromString

topLevel :: P TopLevel
topLevel = do
    whiteSpace
    many topLevelItem <* eof

topLevelItem :: P (AST PosT)
topLevelItem =
    choice [ AstOpen <$> open <?> "open"
           , AstModule <$> module' <?> "module"
           , AstSignature <$> signature <?> "signature"
           , AstDecl <$> declaration <?> "declaration"
           ]


-- Top-level

open :: P (Open PosT)
open = do
    reserved "open"
    i <- qualifiedModule
    a <- optional (reserved "as" *> moduleName)
    h <- optional (reserved "hiding" *> parens (optional comma *> ident `sepBy` comma))
    pure (Open i a h)

signature :: P (Signature PosT)
signature = do
    reserved "signature"
    i <- qualifiedModule
    b <- braces (many abstractDecl)
    pure (Signature i b)

module' :: P (Module PosT)
module' = do
    reserved "module"
    i <- qualifiedModule
    b <- braces (many topLevelItem)
    pure (Module i b)

typeclass :: P (Typeclass PosT)
typeclass = do
    reserved "typeclass"
    i <- typeclassName
    b <- braces (many functionOrSig)
    pure (Typeclass i b)

instance' :: P (Instance PosT)
instance' = do
    reserved "instance"
    c <- qualifiedTypeclass
    t <- nominalType <|> parens typeRef
    b <- braces (many function)
    pure (Instance c t b)

abstractDecl :: P (Decl PosT)
abstractDecl =
    choice [ absType, DecFunSig <$> functionSig ]
  where
    absType = do
        reserved "type"
        i <- typeName
        ps <- many typeParameter <?> "type parameter"
        pure (DecAbsType (AbsType i ps))

declaration :: P (Decl PosT)
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
    decType :: P (Decl PosT)
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

constructor :: P (Ctor PosT)
constructor =
    parser <?> "constructor"
  where
    parser = do
        i <- ctorName
        ts <- many typeRef
        pure (Ctor i ts)

function :: P (Function PosT)
function = do
    reserved "let"
    i <- ident
    ps <- many fnParam <?> "parameters"
    t <- optional (try colon *> typeRef) <?> "type annotation"
    equals
    d <- functionBody
    pure (Function i ps t d)

functionBody :: P (Expr PosT)
functionBody =
    choice [ ELit <$> recordLit            <* optional semi
           , fnLit                         <* optional semi
           , ELit <$> stringLit            <* optional semi
           , ELit <$> charLit              <* optional semi
           , ELit <$> listLiteral          <* optional semi
           , EMatch <$> match              <* optional semi
           , EVar <$> (ident <|> ctorName) <* optional semi
           , hole                          <* optional semi
           , ELit <$> numberLit            <* optional semi
           , parens expr                   <* optional semi
           , expr <* semi
           ]

functionSig :: P (FunctionSig PosT)
functionSig = do
    reserved "let"
    i <- ident
    colon <?> "type annotation"
    t <- typeRef
    pure (FunctionSig i t)

functionOrSig :: P (FnOrSig PosT)
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


fnParam :: P (FnParam PosT)
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



recordType :: P (RecordType PosT)
recordType = do
    reserved "record"
    i <- typeName
    ts <- many typeParameter
    equals
    fs <- recordFields
    pure (RecordType i ts fs)


-- Types

typeRef :: P (Type PosT)
typeRef =
    Parser.buildExpressionParser operators tyTerm <?> "type"
  where
    tyTerm =
        choice [ parens typeRef
               , forallType
               , constraintType
               , nominalType
               , structuralType
               , typeVariable
               ]

    operators = [ [Parser.Infix appParser Parser.AssocLeft]
                , [Parser.Infix (rarrow *> arrowParser) Parser.AssocRight]
                ]

    -- KLUDGE: Getting the span is tricky in these operator parsers. Just use
    -- the span of the first argument.

    appParser = pure $ \t1 t2 ->
        TyApp (position t1) t1 t2

    arrowParser = pure $ \t1 t2 ->
        TyArrow (position t1) t1 t2


forallType :: P (Type PosT)
forallType = do
    p <- pos
    reserved "forall"
    binders <- NonEmpty.fromList <$> some typeParameter
    dot
    t <- typeRef
    pure (TyForall p binders t)

constraintType :: P (Type PosT)
constraintType =
    (parens parser <|> parser) <?> "constraint"
  where
    parser = do
        p <- pos
        cs <- try constraints
        t <- typeRef
        pure (TyConstraint p cs t)

    constraints = do
        res <- NonEmpty.fromList <$> constraint `sepBy1` comma
        reserved "=>"
        pure res

    constraint = do
        ctor <- typeclassName
        ps <- NonEmpty.fromList <$> some typeParameter
        pure (Constraint ctor ps)

typeVariable :: P (Type PosT)
typeVariable = do
    p <- pos
    i <- ident
    pure (TyVar p i)

structuralType :: P (Type PosT)
structuralType = do
    p <- pos
    fs <- recordFields
    pure (TyStructural p fs)

nominalType :: P (Type PosT)
nominalType = do
    p <- pos
    t <- qualifiedType
    pure (TyNominal p t)

recordFields :: P [Field PosT]
recordFields =
    braces (optional comma *> field `sepBy1` comma)
  where
    field :: P (Field PosT)
    field = do
        i <- ident <?> "field name"
        colon <?> "type annotation"
        t <- typeRef
        pure (Field i t)

typeParameter :: P (TypeParameter PosT)
typeParameter = do
    let style = identStyle & styleStart .~ lower
    p <- pos
    s <- Trifecta.ident style
    pure (TypeParameter (Ident p s))


-- Expressions

expr :: P (Expr PosT)
expr = do
    p <- pos
    e <- Parser.buildExpressionParser operators exprTerm <?> "expression"
    t <- exprTypeAnnotation
    pure (maybe e (\t -> EAnnotation (Annotation p e t)) t)
  where
    operators = [ [Parser.Infix parseApp Parser.AssocLeft]
                , [Parser.Infix parseOp Parser.AssocRight]
                ]
    parseApp =
        pure (\x y -> EApp (App x y))

    parseOp = do
        p <- pos
        s <- token (some (oneOf "<>/+-^.=!~@"))
        pure $ \x y ->
          let op = EVar (Ident p (Text.pack s))
          in EApp (App (EApp (App op x)) y)


-- Expressions are a superset of the terms allowed in guards.

exprTerm :: P (Expr PosT)
exprTerm =
    choice [ guardTerm
           , EIf <$> ifThenElse
           ]

guardTerm :: P (Expr PosT)
guardTerm =
    choice [ parens expr
           , ELet <$> let'
           , ELit <$> recordLit
           , fnLit
           , openExpr
           , ELit <$> stringLit
           , ELit <$> charLit
           , ELit <$> listLiteral
           , EMatch <$> match
           , EVar <$> (ident <|> ctorName)
           , hole
           , ELit <$> numberLit
           ]
  where
    openExpr = do
        o <- open
        b <- expr
        pure (EOpen o b)


charLit :: P (Lit PosT)
charLit = LitChar <$> charLiteral

recordLit :: P (Lit PosT)
recordLit =
    LitRecord <$> braces (optional comma *> field `sepEndBy` comma)
  where
    field :: P (Ident PosT, Expr PosT)
    field = do
        f <- ident
        colon <?> "field value"
        e <- expr
        pure (f, e)

let' :: P (Let PosT)
let' = do
    reserved "let"
    d <- discriminator
    equals
    e <- expr <* semi
    b <- expr
    pure (Let d e b)

match :: P (Match PosT)
match = do
    reserved "match"
    e <- expr
    reserved "with"
    ps <- patterns
    pure (Match e ps)

exprTypeAnnotation :: P (Maybe (Type PosT))
exprTypeAnnotation = do
    c <- optional colon <?> "type annotation"
    case c of
      Nothing -> pure Nothing
      Just _ -> Just <$> tyParser <?> "type"
  where
    tyParser = parens typeRef <|> nominalType <|> structuralType <|> typeVariable


ifThenElse :: P (If PosT)
ifThenElse = do
    reserved "if"
    i <- expr
    t <- reserved "then" *> expr
    e <- reserved "else" *> expr
    pure (If i t e)

fnLit :: P (Expr PosT)
fnLit = do
    reserved "fn"
    EFn <$> choice [bare, patterns]
  where
    bare = do
        p <- try pat
        pure [p]

hole :: P (Expr PosT)
hole = do
    let holeStyle = identStyle & styleStart .~ (letter <|> char '_')
    p <- pos
    s <- Trifecta.ident holeStyle
    pure (EHole (Ident p s))


numberLit :: P (Lit PosT)
numberLit = do
    n <- try (runUnspaced integerOrScientific)
    -- Inspect the next char in the stream to ensure unexpected suffixes are
    -- rejected.
    next <- choice [eof *> pure '0', Parser.lookAhead anyChar]
    if Char.isLetter next
      then unexpected ("number suffix " <> "'" <> [next] <> "'")
      else token (pure (either LitInt LitScientific n))


stringLit :: P (Lit PosT)
stringLit =
    parser <?> "string"
  where
    parser :: P (Lit PosT)
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

listLiteral :: P (Lit PosT)
listLiteral =
    parser <?> "list"
  where
    parser =
      LitList <$> brackets (optional comma *> expr `sepEndBy` comma)


-- Pattern matching

patterns :: P [Pat PosT]
patterns = braces (optional pipe *> pat `sepBy` pipe)

pat :: P (Pat PosT)
pat = do
    d <- discriminator
    g <- optional guard
    e <- rarrow *> expr
    pure (Pat d g e)

guard :: P (Guard PosT)
guard =
    choice [ IfGuard <$> (reserved "if" *> guardExpr)
           , UnlessGuard <$> (reserved "unless" *> guardExpr)
           ]
     <?> "pattern guard"
  where
    guardExpr :: P (Expr PosT)
    guardExpr = do
        e <- Parser.buildExpressionParser operators guardTerm <?> "expression"
        t <- exprTypeAnnotation
        pure (maybe e (\t -> EAnnotation (Annotation (position t) e t)) t)
      where
        operators = [[Parser.Infix parseApp Parser.AssocLeft]]
        parseApp = pure (\x y -> EApp (App x y))

discriminator :: P (Discriminator PosT)
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
        p <- pos
        s <- Trifecta.ident wildcardStyle <?> "wildcard"
        pure (DWildcard (Ident p s))

    operators = [ [Parser.Infix (reserved "as" *> pure DAs) Parser.AssocLeft]
                , [Parser.Infix (pure DApp) Parser.AssocLeft]
                ]


-- Identifiers

ctorName :: P (Ident PosT)
ctorName =
    parser <?> "constructor name"
  where
    parser = do
        let style = identStyle & styleStart .~ upper
        p <- pos
        s <- Trifecta.ident style
        pure (Ident p s)

typeclassName :: P (Ident PosT)
typeclassName =
    moduleName
    <?> "typeclass name"

qualifiedTypeclass :: P (QualId PosT)
qualifiedTypeclass = qualifiedModule <?> "typeclass name"

qualifiedModule :: P (QualId PosT)
qualifiedModule =
    parser <?> "module ID"
  where
    parser = QualId . NonEmpty.fromList <$> moduleName `sepBy1` dot


qualifiedType :: P (QualId PosT)
qualifiedType =
    parser <?> "type ID"
  where
    parser = QualId . NonEmpty.fromList <$> typeName `sepBy1` dot

typeName :: P (Ident PosT)
typeName = ctorName
    <?> "type name"

moduleName :: P (Ident PosT)
moduleName = do
    let style =
            identStyle
               & styleStart  .~ upper
               & styleLetter .~ (alphaNum <|> oneOf "_")
    p <- pos
    s <- Trifecta.ident style
    pure (Ident p s)

-- TODO: Accept qualified var references: (modid '.')* id
ident :: P (Ident PosT)
ident = do
    p <- pos
    s <- Trifecta.ident identStyle
    pure (Ident p s)

reserved :: String -> P ()
reserved s = reserve identStyle s <?> s


-- Syntactic elements
--
-- Some of these are redefinitions of the ones provided by Text.Parser to
-- provide clearer errors.

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

semi :: P ()
semi =
    op ";"
    <?> "semicolon"

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
