{-# LANGUAGE TemplateHaskell #-}
module Language.Whippet.AST where

import           Control.Lens.TH
import           Data.Text       (Text)
import qualified Data.Text       as Text

-- Identifiers

data Ident s = Ident {
      _identPos   :: s
    , _identLabel :: Text
    }
    deriving (Show, Eq, Ord)

makeLenses ''Ident
makePrisms ''Ident

-- Type Identifiers

data Type s = Type {
      _typePos   :: s
    , _typeLabel :: Text
    }
    deriving (Show, Eq, Ord)

makeLenses ''Type
makePrisms ''Type

-- Type Parameters

data TypeParameter s = TypeParameter {
      _typeParameterPos   :: s
    , _typeParameterLabel :: Text
    }
    deriving (Show, Eq, Ord)

makeLenses ''TypeParameter
makePrisms ''TypeParameter

-- Declarations

data Decl s = Decl s
    deriving (Show, Eq, Ord)

makeLenses ''Decl
makePrisms ''Decl

-- Constructors

data Ctor s = Ctor {
      _ctorSpan   :: s
    , _ctorIdent  :: Ident s
    , _ctorParams :: [Type s]
    }
    deriving (Show, Eq, Ord)

makeLenses ''Ctor
makePrisms ''Ctor

-- Record Fields

data FieldDecl s = FieldDecl {
      _fieldDeclSpan  :: s
    , _fieldDeclIdent :: Ident s
    , _fieldDeclType  :: Type s
    }
    deriving (Show, Eq, Ord)

makeLenses ''FieldDecl
makePrisms ''FieldDecl

-- Declarations

data AST s
    = AstModule       s (Ident s) [Decl s]
    | AstSignature    s (Ident s) [Decl s]
    | AstAbstractType s (Ident s) [TypeParameter s]
    | AstDataType     s (Ident s) [TypeParameter s] [Ctor s]
    | AstRecordType   s (Ident s) [TypeParameter s] [FieldDecl s]
    deriving (Show, Eq, Ord)

makeLenses ''AST
makePrisms ''AST

-- Utilities

astIdentifier :: AST s -> Maybe (Ident s)
astIdentifier (AstModule _ i _)       = Just i
astIdentifier (AstSignature _ i _)    = Just i
astIdentifier (AstAbstractType _ i _) = Just i
astIdentifier (AstDataType _ i _ _)   = Just i
astIdentifier (AstRecordType _ i _ _) = Just i
