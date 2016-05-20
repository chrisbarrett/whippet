{-# LANGUAGE StandaloneDeriving #-}
module Language.Whippet.AST where

import           Data.Text (Text)
import qualified Data.Text as Text

newtype Ident = Ident Text
    deriving (Show, Eq, Ord)

data AST s
    = AstModule s Ident [Decl s]

deriving instance Show s => Show (AST s)
deriving instance Eq s => Eq (AST s)
deriving instance Ord s => Ord (AST s)

data Decl s = Decl s

deriving instance Show s => Show (Decl s)
deriving instance Eq s => Eq (Decl s)
deriving instance Ord s => Ord (Decl s)
