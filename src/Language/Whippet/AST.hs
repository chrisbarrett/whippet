{-# LANGUAGE TemplateHaskell #-}
module Language.Whippet.AST where

import           Control.Lens.TH
import           Data.Text       (Text)
import qualified Data.Text       as Text

-- Identifiers

data Ident s = Ident {
      _identPos   :: !s
    , _identLabel :: !Text
    }
    deriving (Show, Eq, Ord)

makeLenses ''Ident
makePrisms ''Ident

-- Declarations

data Decl s = Decl s
    deriving (Show, Eq, Ord)

makeLenses ''Decl
makePrisms ''Decl

-- Constructors

data Ctor s = Ctor {
      _ctorSpan   :: !s
    , _ctorIdent  :: !(Ident s)
    , _ctorParams :: ![Ident s]
    }
    deriving (Show, Eq, Ord)

makeLenses ''Ctor
makePrisms ''Ctor

-- Declarations

data AST s
    = AstModule s (Ident s) [Decl s]
    | AstSignature s (Ident s) [Decl s]
    | AstType s (Ident s) [Ctor s]
    deriving (Show, Eq, Ord)

makeLenses ''AST
makePrisms ''AST
