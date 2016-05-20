{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
module Language.Whippet.AST where

import           Control.Lens.TH
import           Data.Text       (Text)
import qualified Data.Text       as Text

newtype Ident = Ident Text
    deriving (Show, Eq, Ord)

data Decl s = Decl s

makeLenses ''Decl
makePrisms ''Decl

deriving instance Show s => Show (Decl s)
deriving instance Eq s => Eq (Decl s)
deriving instance Ord s => Ord (Decl s)

data AST s
    = AstModule s Ident [Decl s]
    | AstSignature s Ident [Decl s]

makeLenses ''AST
makePrisms ''AST

deriving instance Show s => Show (AST s)
deriving instance Eq s => Eq (AST s)
deriving instance Ord s => Ord (AST s)
