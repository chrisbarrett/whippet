{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Parser where

import           Control.Applicative
import           Control.Monad.Trans     (MonadIO)
import           Data.Monoid
import qualified Data.Text               as Text
import           Language.Whippet.AST
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
      Just _  -> recordType pos ident tyArgs <|> dataType pos ident tyArgs
      Nothing -> abstractType pos ident tyArgs

  where
    dataType (start, ln) ident tyArgs = do
        cs <- constructor `sepBy1` pipe
        end <- position
        let span = Span start end ln
        pure (AstDataType span ident tyArgs cs)

    recordType (start, ln) ident tyArgs = do
        flds <- braces (fieldDecl `sepBy1` comma)
        end <- position
        let span = Span start end ln
        pure (AstRecordType span ident tyArgs flds)

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
        ps <- pure []
        pure (ident, ps)

fieldDecl :: Parser (FieldDecl Span)
fieldDecl = do
    let parser = (,) <$> (identifier <?> "field name") <*> (colon *> type')
    ((id, ty) :~ span) <- spanned parser
    pure (FieldDecl span id ty)

typeParameter :: Parser (TypeParameter Span)
typeParameter = do
    (id :~ span) <- spanned (token parser)
    pure (TypeParameter span id)
  where
      parser = do
          c <- lower
          cs <- many (alphaNum <|> char '_')
          pure (Text.pack (c : cs))
        <?> "type parameter"

type' :: Parser (Type Span)
type' = do
    (id :~ span) <- spanned (token parser)
    pure (Type span id)
  where
    parser = do
        c <- upper
        cs <- many (alphaNum <|> oneOf "_-")
        pure (Text.pack (c : cs))
      <?> "type"

typeName :: Parser (Ident Span)
typeName = do
    (id :~ span) <- spanned (token parser)
    pure (Ident span id)
  where
    parser = do
        c <- upper
        cs <- many (alphaNum <|> oneOf "_-")
        pure (Text.pack (c : cs))
      <?> "type name"

identifier :: Parser (Ident Span)
identifier = do
    (id :~ span) <- spanned (token parser)
    pure (Ident span id)
  where
    parser = do
        c <- lower
        cs <- many (alphaNum <|> oneOf "_-?")
        pure (Text.pack (c : cs))
      <?> "identifier"

decls :: Parser [Decl Span]
decls = pure []

reserved :: String -> Parser ()
reserved = reserve style

pipe :: Parser ()
pipe = reserved "|"

equals :: Parser ()
equals = reserved "="

style = emptyIdents {_styleReserved = reservedChars}
reservedChars = ["module", "signature", "type"]
