{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Frontend.Parser where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Trans           (MonadIO)
import           Data.ByteString.Internal      as BSI
import           Data.ByteString.Lazy          as BS
import           Data.Monoid
import           Data.String                   (fromString)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Language.Whippet.Frontend.AST
import           Text.Parser.LookAhead         (lookAhead)
import           Text.Parser.Token.Style
import           Text.Trifecta                 hiding (ident)
import qualified Text.Trifecta                 as Trifecta
import qualified Text.Trifecta.Delta           as Trifecta

-- * Parser definitions

parseFile :: MonadIO m => FilePath -> m (Result (AST Span))
parseFile = parseFromFileEx ast

parseString :: String -> Result (AST Span)
parseString =
    Trifecta.parseByteString ast mempty . fromString

ast :: Parser (AST Span)
ast = whiteSpace *> (astModule <|> astSignature <|> astDecl)

astSignature :: Parser (AST Span)
astSignature = do
    reserved "signature"
    AstSignature <$> ident <*> braces (many declaration)
    <?> "signature"

astModule :: Parser (AST Span)
astModule = do
    reserved "module"
    AstModule <$> ident <*> braces (many ast)
    <?> "module"

astDecl :: Parser (AST Span)
astDecl = AstDecl <$> declaration

decType :: Parser (Decl Span)
decType = do
    id <- reserved "type" *> ident
    tyArgs <- many typeParameter
    eq <- optional equals
    case eq of
      Just _  -> concreteType id tyArgs
      Nothing -> abstractType id tyArgs
    <?> "type declaration"

  where
    concreteType id tyArgs = do
        cs <- optional pipe *> constructor `sepBy1` pipe
        pure (DecDataType id tyArgs cs)

    abstractType id tyArgs = do
        pure (DecAbsType id tyArgs)


constructor :: Parser (Ctor Span)
constructor = do
    i <- ident
    ts <- many typeRef
    pure (Ctor i ts)
    <?> "constructor"

declaration :: Parser (Decl Span)
declaration = decFun <|> decRecord <|> decType

decFun :: Parser (Decl Span)
decFun = do
    reserved "let"
    i <- ident
    t <- colon *> typeRef
    pure (DecFun i t)
    <?> "let"

decRecord :: Parser (Decl Span)
decRecord = do
    i <- reserved "record" *> ident
    ts <- many typeParameter
    fs <- equals *> recordFields
    pure (DecRecordType i ts fs)
    <?> "record declaration"


typeRef :: Parser (Type Span)
typeRef = do
    parens parser <|> parser
  where
    parser = structuralType <|> nominalType

    structuralType = do
        p <- startPos
        flds <- recordFields
        next <- optional moreTyArgs
        span <- spanFromStart p
        let structType = TyStructural span flds
        case next of
          Just next -> pure (TyFun span structType next)
          Nothing   -> pure structType

    nominalType = do
        p <- startPos
        id <- ident
        ps <- many (nominalType <|> parser)
        next <- optional moreTyArgs
        span <- spanFromStart p
        let tyNom = TyNominal span id ps
        case next of
          Just next -> pure (TyFun span tyNom next)
          Nothing   -> pure tyNom

    moreTyArgs = do
        arrow
        p <- startPos
        cur <- typeRef
        next <- optional moreTyArgs
        span <- spanFromStart p
        case next of
          Just next -> pure (TyFun span cur next)
          Nothing   -> pure cur

recordFields :: Parser [Field Span]
recordFields = braces (optional comma *> field `sepBy1` comma)

field :: Parser (Field Span)
field = do
    i <- ident <?> "field name"
    t <- colon *> typeRef
    pure (Field i t)


-- * Token types

typeParameter :: Parser (TypeParameter Span)
typeParameter =
    TypeParameter <$> ident
      <?> "type parameter"

-- * Helpers

style :: IdentifierStyle Parser
style = emptyIdents
        & styleReserved .~ reservedWords
        & styleStart .~ (letter <|> char '_')
        & styleLetter .~ (alphaNum <|> oneOf "_?")
  where
    reservedWords = ["module", "astSignature", "type", "record", "let"]

ident :: Parser (Ident Span)
ident = do
    (s :~ span) <- spanned (Trifecta.ident style)
    pure (Ident span s)

reserved :: String -> Parser ()
reserved = reserve style

pipe :: Parser ()
pipe = reserved "|"

equals :: Parser ()
equals = reserved "="

arrow :: Parser ()
arrow = reserved "->"

-- * Source position utilities

data Start = Start Trifecta.Delta BSI.ByteString

startPos :: DeltaParsing m => m Start
startPos = do
    p <- position
    l <- line
    pure (Start p l)

spanFromStart :: DeltaParsing m => Start -> m Span
spanFromStart (Start s l) = do
    e <- position
    pure (Span s e l)
