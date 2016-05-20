module Language.Whippet.Parser where

import           Control.Monad.Trans  (MonadIO)
import           Language.Whippet.AST
import           Text.Trifecta

parseFile :: MonadIO m => FilePath -> m (Result (AST Span))
parseFile = parseFromFileEx parseModule

parseModule :: Parser (AST Span)
parseModule = undefined
