{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Frontend.Parser where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Trans           (MonadIO)
import           Data.Monoid
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Language.Whippet.Frontend.AST
import           Text.Parser.Token.Style
import           Text.Trifecta

parseFile :: MonadIO m => FilePath -> m (Result (AST Span))
parseFile = parseFromFileEx topLevel

topLevel :: Parser (AST Span)
topLevel = whiteSpace *> (module' <|> signature <|> typeDecl)

module' :: Parser (AST Span)
module' = do
    let parser = reserved "module" *> ((,) <$> typeName <*> braces decls)
    ((id, ds) :~ span) <- spanned parser
    pure (AstModule span id ds)

signature :: Parser (AST Span)
signature = do
    ((id, ds) :~ span) <- spanned parser
    pure (AstSignature span id ds)
  where
    parser = reserved "signature" *> ((,) <$> typeName <*> braces decls)

typeDecl :: Parser (AST Span)
typeDecl = do
    s <- position
    l <- line
    let pos = (s, l)

    reserved "type"
    ident <- typeName
    tyArgs <- many typeParameter

    eq <- optional equals
    case eq of
      Just _  -> concreteType pos ident tyArgs
      Nothing -> abstractType pos ident tyArgs

  where
    concreteType (start, ln) ident tyArgs = do
        cs <- optional pipe *> constructor `sepBy1` pipe
        end <- position
        let span = Span start end ln
        pure (AstDataType span ident tyArgs cs)

    abstractType (start, ln) ident tyArgs = do
        end <- position
        let span = Span start end ln
        pure (AstAbstractType span ident tyArgs)

constructor :: Parser (Ctor Span)
constructor = do
    ((id, ps) :~ span) <- spanned parser
    pure (Ctor span id ps)
  where
    parser = do
        ident <- typeName
        ps <- many type'
        pure (ident, ps)

field :: Parser (Field Span)
field = do
    let parser = (,) <$> (identifier' <?> "field name")
                     <*> (colon *> type'
                         <?> "type")
    ((id, ty) :~ span) <- spanned parser
    pure (Field span id ty)

decls :: Parser [Decl Span]
decls = pure []


-- Token types

typeParameter :: Parser (TypeParameter Span)
typeParameter = do
    TypeParameter <$> tokenLike Ident ((:) <$> lower <*> many (alphaNum <|> oneOf "_"))
      <?> "type parameter"

type' :: Parser (Type Span)
type' = do
    start <- position
    ln <- line
    let mkSpan = \end -> Span start end ln
    structuralType mkSpan <|> nominalType mkSpan
  where
    nominalType mkSpan = do
        i <- (:) <$> letter <*> many (alphaNum <|> oneOf "_")
        s <- mkSpan <$> position
        pure (TyNominal s (Ident s (Text.pack i)))
        <?> "type name"

    structuralType mkSpan = do
        flds <- braces (field `sepBy1` comma)
        end <- position
        pure (TyStructural (mkSpan end) flds)


typeName :: Parser (Ident Span)
typeName =
    tokenLike Ident ((:) <$> upper <*> many (alphaNum <|> oneOf "_"))
      <?> "type name"

identifier' :: Parser (Ident Span)
identifier' = do
    tokenLike Ident ((:) <$> lower <*> many (alphaNum <|> oneOf "_-?"))
      <?> "type name"

tokenLike :: (Span -> Text -> t) -> Parser String -> Parser t
tokenLike f p = do
    (id :~ span) <- spanned (token (Text.pack <$> p))
    pure (f span id)


-- Helpers

reserved :: String -> Parser ()
reserved = reserve style

pipe :: Parser ()
pipe = reserved "|"

equals :: Parser ()
equals = reserved "="

style = emptyIdents {_styleReserved = reservedChars}
reservedChars = ["module", "signature", "type"]
