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

-- Record Fields

data FieldDecl s = FieldDecl {
      _fieldDeclSpan   :: !s
    , _fieldDeclIdent  :: !(Ident s)
    , _fieldDeclParams :: ![Ident s]
    }
    deriving (Show, Eq, Ord)

makeLenses ''FieldDecl
makePrisms ''FieldDecl

-- Declarations

data AST s
    = AstModule
          s
          -- ^ Src pos
          (Ident s)
          -- ^ module name
          [Decl s]
          -- ^ inner declarations
    | AstSignature
          s
          -- ^ Src pos
          (Ident s)
          -- ^ signature name
          [Decl s]
          -- ^ inner declarations
    | AstType
          s
          -- ^ Src pos
          (Ident s)
          -- ^ Type name
          [Ident s]
          -- ^ Type parameters
          [Ctor s]
          -- ^ Constructors
    | AstRecordType
          s
          -- ^ Src pos
          (Ident s)
          -- ^ Type name
          [Ident s]
          -- ^ Type parameters
          [FieldDecl s]
          -- ^ Fields

    deriving (Show, Eq, Ord)

makeLenses ''AST
makePrisms ''AST
