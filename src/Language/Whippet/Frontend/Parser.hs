{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Frontend.Parser where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Trans           (MonadIO)
import           Data.ByteString.Internal      as BSI
import           Data.ByteString.Lazy          as BS
import qualified Data.Maybe                    as Maybe
import           Data.Monoid                   ((<>))
import           Data.String                   (fromString)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Language.Whippet.Frontend.AST
import           Text.Parser.Expression
import           Text.Parser.LookAhead         (lookAhead)
import           Text.Parser.Token.Style
import           Text.Trifecta                 hiding (ident)
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
astSignature = do
    reserved "signature"
    AstSignature <$> ident <*> braces (many declaration)
    <?> "signature"

astModule :: Parser AST
astModule = do
    reserved "module"
    AstModule <$> ident <*> braces (many ast)
    <?> "module"

astDecl :: Parser AST
astDecl = AstDecl <$> declaration

decType :: Parser Decl
decType = do
    reserved "type"
    id <- ident
    tyArgs <- many typeParameter
    concreteType id tyArgs <|> abstractType id tyArgs
    <?> "type declaration"

  where
    concreteType id tyArgs = do
        equals
        optional pipe
        DecDataType id tyArgs <$> constructor `sepBy1` pipe

    abstractType id tyArgs = do
        pure (DecAbsType id tyArgs)


constructor :: Parser Ctor
constructor = do
    i <- ident
    ts <- many typeRef
    pure (Ctor i ts)
    <?> "constructor"

declaration :: Parser Decl
declaration = decFun <|> decRecord <|> decType

decFun :: Parser Decl
decFun = do
    reserved "let"
    i <- ident
    colon
    t <- typeRef
    pure (DecFun i t Nothing)
    <?> "let"

decRecord :: Parser Decl
decRecord = do
    reserved "record"
    i <- ident
    ts <- many typeParameter
    equals
    fs <- recordFields
    pure (DecRecordType i ts fs)
    <?> "record declaration"


typeRef :: Parser Type
typeRef = do
    buildExpressionParser operators tyTerm
  where
    tyTerm =
        parens typeRef <|> nominalType <|> structuralType

    structuralType =
        TyStructural <$> recordFields
        <?> "structural type"

    nominalType = do
        id <- ident <?> "type name"
        pure (TyNominal id)

    operators = [ [Infix (pure TyApp) AssocLeft]
                , [Infix (rarrow *> pure TyFun) AssocRight]
                ]


recordFields :: Parser [Field]
recordFields = braces (optional comma *> field `sepBy1` comma)

field :: Parser Field
field = do
    i <- ident <?> "field name"
    colon
    t <- typeRef
    pure (Field i t)

typeParameter :: Parser TypeParameter
typeParameter = TypeParameter <$> ident <?> "type parameter"

-- * Expression

expr :: Parser Expr
expr =
    buildExpressionParser [] term
  where
    term = variable

    variable =
        choice [ EVar <$> ident
               , lookAhead (char '?') *> fail "Identifier cannot start with a question mark"
               ]

-- * Helpers

style :: Trifecta.IdentifierStyle Parser
style = emptyIdents
        & styleReserved .~ reservedWords
        & styleStart    .~ (letter <|> char '_')
        & styleLetter   .~ (alphaNum <|> oneOf "_?")
  where
    reservedWords = ["module", "astSignature", "type", "record", "let"]

ident :: Parser Ident
ident = do
    (s :~ span) <- spanned (Trifecta.ident style)
    pure (Ident span s)

reserved :: String -> Parser ()
reserved s = reserve style s <?> s

pipe :: Parser ()
pipe = reserved "|"

equals :: Parser ()
equals = reserved "="

rarrow :: Parser ()
rarrow = reserved "->"
