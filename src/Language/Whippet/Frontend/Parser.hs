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
import           Text.Trifecta                 hiding (ident, stringLiteral)
import qualified Text.Trifecta                 as Trifecta
import qualified Text.Trifecta.Delta           as Trifecta

-- * Parser definitions

parseFile :: MonadIO m => FilePath -> m (Result AST)
parseFile = parseFromFileEx ast

parseString :: String -> Result AST
parseString =
    Trifecta.parseByteString ast mempty . fromString

ast :: Parser AST
ast = do
    whiteSpace
    astOpen <|> astModule <|> astSignature <|> astDecl

-- * Top-level

astOpen :: Parser AST
astOpen =
    AstOpen <$> parseOpen <?> "open statement"

parseOpen :: Parser Open
parseOpen = do
    reserved "open"
    i <- qualifiedModule
    a <- optional (reserved "as" *> moduleName)
    h <- optional (reserved "hiding" *> parens (optional comma *> ident `sepBy` comma))
    pure (Open i a h)

astSignature :: Parser AST
astSignature =
    parser <?> "signature"
  where
    parser = do
        reserved "signature"
        AstSignature <$> qualifiedModule <*> braces (many declaration)

astModule :: Parser AST
astModule =
    parser <?> "module"
  where
    parser = do
        reserved "module"
        AstModule <$> qualifiedModule <*> braces (many ast)

decTypeclass :: Parser Decl
decTypeclass =
    parser <?> "typeclass"
  where
    parser = do
        reserved "typeclass"
        DecTypeclass <$> typeclassName <*> braces (many decFun)

astDecl :: Parser AST
astDecl = AstDecl <$> declaration

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
        DecDataType id tyArgs <$> constructor `sepBy1` pipe

    abstractType id tyArgs =
        pure (DecAbsType id tyArgs)


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
    parser = decFun <|> decRecord <|> decType <|> decTypeclass

decFun :: Parser Decl
decFun =
    parser <?> "let declaration"
  where
    parser = do
        reserved "let"
        i <- ident
        colon
        t <- typeRef
        pure (DecFun i t Nothing)

decRecord :: Parser Decl
decRecord =
    parser <?> "record declaration"
  where
    parser = do
        reserved "record"
        i <- typeName
        ts <- many typeParameter
        equals
        fs <- recordFields
        pure (DecRecordType i ts fs)


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
    colon
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
    pure (maybe e (EAnnotation e) t)

  where
    operators = [[Infix (pure EApp) AssocLeft]]
    term =  fn
        <|> ifThenElse
        <|> openExpr
        <|> stringLiteral
        <|> char
        <|> listLiteral
        <|> recordLiteral
        <|> match
        <|> let'
        <|> variable
        <|> hole
        <|> numberLiteral

    variable = EVar <$> ident
    char = ELit . LitChar <$> charLiteral

openExpr :: Parser Expr
openExpr =
    parser <?> "open expression"
  where
    parser = do
        o <- parseOpen
        reserved "in"
        b <- expr
        pure (EOpen o b)

let' :: Parser Expr
let' =
    parser <?> "let expression"
  where
    parser = do
        reserved "let"
        d <- discriminator
        equals
        e <- expr
        reserved "in"
        b <- expr
        pure (ELet d e b)

match :: Parser Expr
match =
    parser <?> "match expression"
  where
    parser = do
        reserved "match"
        e <- expr
        reserved "with"
        ps <- patterns
        pure (EMatch e ps)

typeAnnotation :: Parser Type
typeAnnotation =
    parser <?> "type annotation"
  where
    parser = do
      colon
      parens typeRef <|> nominalType <|> structuralType <|> typeVariable

ifThenElse :: Parser Expr
ifThenElse =
    parser <?> "if expression"
  where
    parser =
        EIf <$> (reserved "if" *> expr)
            <*> (reserved "then" *> expr)
            <*> (reserved "else" *> expr)

fn :: Parser Expr
fn =
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


numberLiteral :: Parser Expr
numberLiteral =
    parser <?> "number"
  where
    parser = do
        n <- integerOrScientific
        pure (ELit (either LitInt LitScientific n))

stringLiteral :: Parser Expr
stringLiteral =
    parser <?> "string"
  where
    parser = do
        let start   = char '"' <?> "start of string (double-quotes)"
            content = (escapeSequence <|> anyChar) <?> "string content"
            end     = char '"' <?> "end of string"
        str <- start *> token (content `manyTill` end)
        pure ((ELit . LitString) (Text.pack str))
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

listLiteral :: Parser Expr
listLiteral =
    parser <?> "list"
  where
    parser =
      ELit . LitList <$> brackets (optional comma *> expr `sepEndBy` comma)

recordLiteral :: Parser Expr
recordLiteral =
    parser <?> "record"
  where
    parser = ELit . LitRecord <$> braces (optional comma *> field `sepEndBy` comma)

    field :: Parser (Ident, Expr)
    field = do
        f <- ident
        colon
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
                , "in"
                , "then"
                , "else"
                , "fn"
                , "as"
                , "match"
                , "with"
                , "open"
                , "hiding"
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

qualifiedModule :: Parser QualId
qualifiedModule =
    parser <?> "module ID"
  where
    parser = QualId . NonEmpty.fromList <$> moduleName `sepBy1` dot

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
