{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Parser where

import           Control.Applicative
import           Control.Monad.Trans     (MonadIO)
import           Language.Whippet.AST
import           Text.Parser.Token.Style
import           Text.Trifecta

parseFile :: MonadIO m => FilePath -> m (Result (AST Span))
parseFile = parseFromFileEx (module' <|> signature)

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

identifier :: Parser Ident
identifier = Ident <$> ident style

decls :: Parser [Decl Span]
decls = pure []

reserved :: String -> Parser ()
reserved = reserve style

style = emptyIdents {_styleReserved = reservedChars}
reservedChars = ["module", "signature"]
