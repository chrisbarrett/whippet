{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
module Language.Whippet.AST where

import           Control.Lens.TH
import           Data.Text       (Text)
import qualified Data.Text       as Text

data Ident s = Ident {
      _identPos  :: !s
    , _identText :: !Text
    }
    deriving (Show, Eq, Ord)

makeLenses ''Ident
makePrisms ''Ident

data Decl s = Decl s

makeLenses ''Decl
makePrisms ''Decl

deriving instance Show s => Show (Decl s)
deriving instance Eq s => Eq (Decl s)
deriving instance Ord s => Ord (Decl s)

data AST s
    = AstModule s (Ident s) [Decl s]
    | AstSignature s (Ident s) [Decl s]
    | AstType s (Ident s)

makeLenses ''AST
makePrisms ''AST

deriving instance Show s => Show (AST s)
deriving instance Eq s => Eq (AST s)
deriving instance Ord s => Ord (AST s)
