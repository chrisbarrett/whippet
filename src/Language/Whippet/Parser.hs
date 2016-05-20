{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Parser where

import           Control.Monad.Trans     (MonadIO)
import           Language.Whippet.AST
import           Text.Parser.Token.Style
import           Text.Trifecta

parseFile :: MonadIO m => FilePath -> m (Result (AST Span))
parseFile = parseFromFileEx parseModule

parseModule :: Parser (AST Span)
parseModule = do
    let parser = reserved "module" *> ((,) <$> identifier <*> braces decls)
    ((id, ds) :~ span) <- spanned parser
    pure (AstModule span id ds)

identifier :: Parser Ident
identifier = Ident <$> ident style

decls :: Parser [Decl Span]
decls = pure []

reserved :: String -> Parser ()
reserved = reserve style

style = emptyIdents {_styleReserved = reservedChars}
reservedChars = ["module"]
