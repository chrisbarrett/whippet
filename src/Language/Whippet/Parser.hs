{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Parser where

import           Control.Applicative
import           Control.Monad.Trans     (MonadIO)
import           Language.Whippet.AST
import           Text.Parser.Token.Style
import           Text.Trifecta

parseFile :: MonadIO m => FilePath -> m (Result (AST Span))
parseFile = parseFromFileEx (module' <|> signature <|> type')

module' :: Parser (AST Span)
module' = do
    let parser = reserved "module" *> ((,) <$> identifier <*> braces decls)
    ((id, ds) :~ span) <- spanned parser
    pure (AstModule span id ds)

signature :: Parser (AST Span)
signature = do
    ((id, ds) :~ span) <- spanned parser
    pure (AstSignature span id ds)
  where
    parser = reserved "signature" *> ((,) <$> identifier <*> braces decls)

type' :: Parser (AST Span)
type' = do
    ((id, ts, cs) :~ span) <- spanned parser
    pure (AstType span id ts cs)
  where
    parser = do
        reserved "type"
        ident <- identifier
        tsOrEmpty <- many identifier
        cs <- optional (equals *> constructor `sepBy1` pipe)
        let csOrEmpty = maybe [] id cs
        pure (ident, tsOrEmpty, csOrEmpty)

constructor :: Parser (Ctor Span)
constructor = do
    ((id, ps) :~ span) <- spanned parser
    pure (Ctor span id ps)
  where
    parser = do
        ident <- identifier
        ps <- pure []
        pure (ident, ps)

identifier :: Parser (Ident Span)
identifier = do
    (id :~ span) <- spanned (ident style)
    pure (Ident span id)

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
