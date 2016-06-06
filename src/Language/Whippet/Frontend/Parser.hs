{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Frontend.Parser where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Trans           (MonadIO)
import           Data.Monoid                   ((<>))
import           Data.String                   (fromString)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
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
ast = whiteSpace *> (astModule <|> astSignature <|> astDecl)

astSignature :: Parser AST
astSignature =
    parser <?> "signature"
  where
    parser = do
      reserved "signature"
      AstSignature <$> ident <*> braces (many declaration)

astModule :: Parser AST
astModule =
    parser <?> "module"
  where
    parser = do
        reserved "module"
        AstModule <$> ident <*> braces (many ast)

astDecl :: Parser AST
astDecl = AstDecl <$> declaration

decType :: Parser Decl
decType =
    parser <?> "type declaration"
  where
    parser = do
        reserved "type"
        id <- ident
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
    parser = decFun <|> decRecord <|> decType

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
        i <- ident
        ts <- many typeParameter
        equals
        fs <- recordFields
        pure (DecRecordType i ts fs)


typeRef :: Parser Type
typeRef =
    buildExpressionParser operators tyTerm
  where
    operators = [ [Infix (pure TyApp) AssocLeft]
                , [Infix (rarrow *> pure TyFun) AssocRight]
                ]

    tyTerm =
        parens typeRef <|> nominalType <|> structuralType

structuralType :: Parser Type
structuralType =
    TyStructural <$> recordFields
    <?> "structural type"

nominalType :: Parser Type
nominalType =
    parser <?> "type name"
  where
    parser = TyNominal <$> ident

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

-- * Expression

expr :: Parser Expr
expr = do
    e <- term <?> "expression"
    t <- optional typeAnnotation
    pure (maybe e (EAnnotation e) t)
  where
    term =  fn
        <|> variable
        <|> ifThenElse
        <|> hole
        <|> numberLiteral
        <|> stringLiteral
        <|> listLiteral
        <|> recordLiteral

    variable = EVar <$> ident

typeAnnotation :: Parser Type
typeAnnotation =
    parser <?> "type annotation"
  where
    parser = do
      colon
      choice [nominalType, structuralType, parens typeRef]

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
        EFn <$> choice [bare, nested]

    nested = braces (optional pipe *> pat `sepBy1` pipe)

    bare = do
        p <- try pat
        pure [p]

discriminator :: Parser Discriminator
discriminator = do
    e <- buildExpressionParser operators discTerm <?> "pattern discriminator"
    t <- optional typeAnnotation
    pure (maybe e (DAnn e) t)
  where
    discTerm = parens discriminator <|> dctor <|> dvar <|> drecord

    dctor = DCtor <$> ctorName
    dvar = DVar <$> ident
    drecord = DRec <$> braces (optional comma *> discriminator `sepBy` comma)

    operators :: OperatorTable Parser Discriminator
    operators = [ [Infix (reserved "as" *> pure DAs) AssocLeft]
                , [Infix (pure DApp) AssocLeft]
                ]


pat :: Parser Pat
pat = do
    d <- discriminator
    e <- rarrow *> expr
    pure (Pat d e)

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
    parser = ELit . either LitInt LitScientific <$> integerOrScientific

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
                ]

ctorName :: Parser Ident
ctorName = do
    let style = identStyle & styleStart .~ upper
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

equals :: Parser ()
equals = reserved "="

rarrow :: Parser ()
rarrow = reserved "->"
