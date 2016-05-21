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
    let parser = reserved "signature" *> ((,) <$> identifier <*> braces decls)
    ((id, ds) :~ span) <- spanned parser
    pure (AstSignature span id ds)

type' :: Parser (AST Span)
type' = do
    let parser = reserved "type" *> identifier
    (id :~ span) <- spanned parser
    pure (AstType span id)

identifier :: Parser (Ident Span)
identifier = do
    (id :~ span) <- spanned (ident style)
    pure (Ident span id)

decls :: Parser [Decl Span]
decls = pure []

reserved :: String -> Parser ()
reserved = reserve style

style = emptyIdents {_styleReserved = reservedChars}
reservedChars = ["module", "signature", "type"]
