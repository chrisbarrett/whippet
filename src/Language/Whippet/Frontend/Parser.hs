{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Frontend.Parser where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Trans           (MonadIO)
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Maybe                    as Maybe
import           Data.Monoid                   ((<>))
import           Data.String                   (fromString)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Tuple                    as Tuple
import           Language.Whippet.Frontend.AST
import           Text.Parser.Expression
import           Text.Parser.LookAhead         (lookAhead)
import           Text.Parser.Token.Style
import           Text.Trifecta                 hiding (ident, stringLit)
import qualified Text.Trifecta                 as Trifecta
import qualified Text.Trifecta.Delta           as Trifecta

-- * Parser definitions

parseFile :: MonadIO m => FilePath -> m (Result TopLevel)
parseFile = parseFromFileEx topLevel

parseString :: String -> Result TopLevel
parseString =
    Trifecta.parseByteString topLevel mempty . fromString

topLevel :: Parser TopLevel
topLevel = whiteSpace *> many ast <* eof

ast :: Parser AST
ast =
    choice [ AstOpen <$> open
           , AstModule <$> module'
           , AstSignature <$> signature
           , AstDecl <$> declaration
           ]

-- * Top-level

open :: Parser Open
open = do
    reserved "open"
    i <- qualifiedModule
    a <- optional (reserved "as" *> moduleName)
    h <- optional (reserved "hiding" *> parens (optional comma *> ident `sepBy` comma))
    pure (Open i a h)

signature :: Parser Signature
signature =
    parser <?> "signature"
  where
    parser = do
        reserved "signature"
        i <- qualifiedModule
        b <- braces (many declaration)
        pure (Signature i b)

module' :: Parser Module
module' =
    parser <?> "module"
  where
    parser = do
        reserved "module"
        Module <$> qualifiedModule <*> braces (many ast)

typeclass :: Parser Typeclass
typeclass =
    parser <?> "typeclass"
  where
    parser = do
        reserved "typeclass"
        Typeclass <$> typeclassName <*> braces (many function)

instance' :: Parser Instance
instance' =
    parser <?> "instance"
  where
    parser = do
        reserved "instance"
        c <- qualifiedTypeclass
        t <- nominalType <|> parens typeRef
        b <- braces (many function)
        pure (Instance c t b)

constructor :: Parser Ctor
constructor =
    parser <?> "constructor"
  where
    parser = do
        i <- ctorName
        ts <- many typeRef
        pure (Ctor i ts)
        <?> "constructor"

declaration :: Parser Decl
declaration =
    parser <?> "declaration"
  where
    parser =
        choice [ DecFun <$> function
               , DecRecordType <$> recordType
               , DecTypeclass <$> typeclass
               , DecInstance <$> instance'
               , decType
               ]

    -- Concrete and abstract types share the same prefix, delay branching in the
    -- parser to improve error messages.
    decType :: Parser Decl
    decType =
        parser <?> "type declaration"
      where
        parser = do
            reserved "type"
            id <- typeName
            tyArgs <- many typeParameter
            concreteType id tyArgs <|> abstractType id tyArgs

        concreteType id tyArgs = do
            equals
            optional pipe
            cs <- constructor `sepBy1` pipe
            pure (DecDataType (DataType id tyArgs cs))

        abstractType id tyArgs =
            pure (DecAbsType (AbsType id tyArgs))


function :: Parser Function
function =
    parser <?> "let declaration"
  where
    parser = do
        reserved "let"
        i <- ident
        ps <- optional (some fnParam <?> "parameters")
        maybe (fnSig i) (fnImpl i) ps

    fnParam :: Parser FnParam
    fnParam =
        parens paramWithTy <|> paramIdent
      where
        paramWithTy = do
            i <- ident
            colon
            t <- typeRef
            pure (FnParam i (Just t))

        paramIdent =
            FnParam <$> ident <*> pure Nothing

    fnSig :: Ident -> Parser Function
    fnSig i = do
        colon <?> "type annotation"
        t <- typeRef
        pure (Function i Nothing (Just t) Nothing)

    fnImpl :: Ident -> [FnParam] -> Parser Function
    fnImpl i ps = do
        t <- optional (colon *> typeRef) <?> "type annotation"
        equals
        e <- expr
        pure (Function i (Just ps) t (Just e))


recordType :: Parser RecordType
recordType =
    parser <?> "record declaration"
  where
    parser = do
        reserved "record"
        i <- typeName
        ts <- many typeParameter
        equals
        fs <- recordFields
        pure (RecordType i ts fs)


-- * Types

typeRef :: Parser Type
typeRef =
    buildExpressionParser operators tyTerm
  where
    operators = [ [Infix (pure TyApp) AssocLeft]
                , [Infix (rarrow *> pure TyArrow) AssocRight]
                ]

    tyTerm =
        parens typeRef <|> nominalType <|> structuralType <|> typeVariable

typeVariable :: Parser Type
typeVariable =
    parser <?> "type variable"
  where
    parser = TyVar <$> ident

structuralType :: Parser Type
structuralType =
    TyStructural <$> recordFields
    <?> "structural type"

nominalType :: Parser Type
nominalType =
    parser <?> "type name"
  where
    parser = TyNominal <$> qualifiedType

recordFields :: Parser [Field]
recordFields = braces (optional comma *> field `sepBy1` comma)

field :: Parser Field
field = do
    i <- ident <?> "field name"
    colon <?> "type annotation"
    t <- typeRef
    pure (Field i t)

typeParameter :: Parser TypeParameter
typeParameter =
    parser <?> "type parameter"
  where
    parser = TypeParameter <$> ident


-- * Expressions

expr :: Parser Expr
expr = do
    e <- buildExpressionParser operators term <?> "expression"
    t <- optional typeAnnotation
    pure (maybe e (EAnnotation . Annotation e) t)

  where
    operators = [[Infix (pure (\x y -> EApp (App x y))) AssocLeft]]
    term =
        choice [ fnLit
               , EIf <$> ifThenElse
               , openExpr
               , ELit <$> stringLit
               , ELit <$> char
               , ELit <$> listLiteral
               , ELit <$> recordLiteral
               , EMatch <$> match
               , ELet <$> let'
               , EVar <$> ident
               , hole
               , ELit <$> numberLit
               ]

    char = LitChar <$> charLiteral

    openExpr =
        parser <?> "open expression"
      where
        parser = do
            o <- open
            semi
            b <- expr
            pure (EOpen o b)

let' :: Parser Let
let' =
    parser <?> "let expression"
  where
    parser = do
        reserved "let"
        d <- discriminator
        equals
        e <- expr
        semi
        b <- expr
        pure (Let d e b)

match :: Parser Match
match =
    parser <?> "match expression"
  where
    parser = do
        reserved "match"
        e <- expr
        reserved "with"
        ps <- patterns
        pure (Match e ps)

typeAnnotation :: Parser Type
typeAnnotation =
    parser <?> "type annotation"
  where
    parser = do
      colon
      parens typeRef <|> nominalType <|> structuralType <|> typeVariable

ifThenElse :: Parser If
ifThenElse =
    parser <?> "if expression"
  where
    parser =
        If <$> (reserved "if" *> expr)
           <*> (reserved "then" *> expr)
           <*> (reserved "else" *> expr)

fnLit :: Parser Expr
fnLit =
    parser <?> "fn"
  where
    parser = do
        reserved "fn"
        EFn <$> choice [bare, patterns]

    bare = do
        p <- try pat
        pure [p]

hole :: Parser Expr
hole =
    parser <?> "hole"
  where
    parser = do
      let holeStyle = identStyle & styleStart .~ (letter <|> char '_')
      (s :~ span) <- spanned (Trifecta.ident holeStyle)
      pure (EHole (Ident span s))


numberLit :: Parser Lit
numberLit =
    parser <?> "number"
  where
    parser = do
        n <- integerOrScientific
        pure (either LitInt LitScientific n)

stringLit :: Parser Lit
stringLit =
    parser <?> "string"
  where
    parser = do
        let start   = char '"' <?> "start of string (double-quotes)"
            content = (escapeSequence <|> anyChar) <?> "string content"
            end     = char '"' <?> "end of string"
        str <- start *> token (content `manyTill` end)
        pure (LitString (Text.pack str))
    escapeSequence = do
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

listLiteral :: Parser Lit
listLiteral =
    parser <?> "list"
  where
    parser =
      LitList <$> brackets (optional comma *> expr `sepEndBy` comma)

recordLiteral :: Parser Lit
recordLiteral =
    parser <?> "record"
  where
    parser = LitRecord <$> braces (optional comma *> field `sepEndBy` comma)

    field :: Parser (Ident, Expr)
    field = do
        f <- ident
        colon <?> "field value"
        e <- expr
        pure (f, e)


-- * Pattern matching

patterns :: Parser [Pat]
patterns = braces (optional pipe *> pat `sepBy1` pipe)

pat :: Parser Pat
pat = do
    d <- discriminator
    e <- rarrow *> expr
    pure (Pat d e)

discriminator :: Parser Discriminator
discriminator = do
    e <- buildExpressionParser operators discTerm <?> "discriminator"
    t <- optional typeAnnotation
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

    operators :: OperatorTable Parser Discriminator
    operators = [ [Infix (reserved "as" *> pure DAs) AssocLeft]
                , [Infix (pure DApp) AssocLeft]
                ]


-- * Helpers

reservedWords = [ "module"
                , "signature"
                , "type"
                , "record"
                , "let"
                , "if"
                , "let"
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
                ]


ctorName :: Parser Ident
ctorName =
    parser <?> "constructor name"
  where
    parser = do
        let style = identStyle & styleStart .~ upper
        (s :~ span) <- spanned (Trifecta.ident style)
        pure (Ident span s)

typeclassName :: Parser Ident
typeclassName = moduleName
    <?> "typeclass name"

qualifiedTypeclass :: Parser QualId
qualifiedTypeclass = qualifiedModule <?> "typeclass name"

qualifiedModule :: Parser QualId
qualifiedModule =
    parser <?> "module ID"
  where
    parser = QualId . NonEmpty.fromList <$> moduleName `sepBy1` dot


-- TODO: Make this parse:              <module>... '.' <type>
qualifiedType :: Parser QualId
qualifiedType =
    parser <?> "type ID"
  where
    parser = QualId . NonEmpty.fromList <$> typeName `sepBy1` dot

typeName :: Parser Ident
typeName = ctorName
    <?> "type name"

moduleName :: Parser Ident
moduleName = do
    let style =
            identStyle
               & styleStart  .~ upper
               & styleLetter .~ (alphaNum <|> oneOf "_")
    (s :~ span) <- spanned (Trifecta.ident style)
    pure (Ident span s)

-- TODO: Make var references qualified:              <module>... '.' <ident>

ident :: Parser Ident
ident = do
    (s :~ span) <- spanned (Trifecta.ident identStyle)
    pure (Ident span s)

identStyle :: Trifecta.IdentifierStyle Parser
identStyle = emptyIdents
        & styleReserved .~ reservedWords
        & styleStart    .~ letter
        & styleLetter   .~ (alphaNum <|> oneOf "_?")

reserved :: String -> Parser ()
reserved s = reserve identStyle s <?> s

pipe :: Parser ()
pipe = reserved "|"
    <?> "pipe"

equals :: Parser ()
equals = reserved "="
    <?> "equals"

rarrow :: Parser ()
rarrow = reserved "->"
    <?> "right arrow"
