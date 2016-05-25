{-# LANGUAGE TemplateHaskell #-}
module Language.Whippet.Frontend.AST where

import           Control.Lens
import           Control.Lens.TH
import           Data.Monoid
import           Data.Text       (Text)

-- Identifiers

data Ident s = Ident {
      _identPos   :: s
    , _identLabel :: Text
    }
    deriving (Eq, Ord)

makeLenses ''Ident
makePrisms ''Ident

instance Show (Ident s) where
  show =  show . view identLabel

-- Type Identifiers

data Type s = Type {
      _typePos   :: s
    , _typeIdent :: Ident s
    }
    deriving (Eq, Ord)

makeLenses ''Type
makePrisms ''Type

instance Show (Type s) where
  show = show . view typeIdent

-- Type Parameters

newtype TypeParameter s = TypeParameter {_typeParameterIdent :: Ident s}
    deriving (Eq, Ord)

makeLenses ''TypeParameter
makePrisms ''TypeParameter

instance Show (TypeParameter s) where
  show = show . view typeParameterIdent

-- Declarations

data Decl s = Decl s
    deriving (Eq, Ord)

makeLenses ''Decl
makePrisms ''Decl

instance Show (Decl s) where
  show _ = ""

-- Constructors

data Ctor s = Ctor {
      _ctorSpan   :: s
    , _ctorIdent  :: Ident s
    , _ctorParams :: [Type s]
    }
    deriving (Eq, Ord)

makeLenses ''Ctor
makePrisms ''Ctor

instance Show (Ctor s) where
  show c = "Ctor {ident=" <> show (c^.ctorIdent)
            <> ", params=" <> show (c^.ctorParams) <> "}"


-- Record Fields

data Field s = Field {
      _fieldSpan  :: s
    , _fieldIdent :: Ident s
    , _fieldType  :: Type s
    }
    deriving (Eq, Ord)

makeLenses ''Field
makePrisms ''Field

instance Show (Field s) where
  show c = "Field {ident=" <> show (c^.fieldIdent)
                  <> ", type=" <> show (c^.fieldType) <> "}"

-- Declarations

data AST s
    = AstModule       s (Ident s) [Decl s]
    | AstSignature    s (Ident s) [Decl s]
    | AstAbstractType s (Ident s) [TypeParameter s]
    | AstDataType     s (Ident s) [TypeParameter s] [Ctor s]
    | AstRecordType   s (Ident s) [TypeParameter s] [Field s]
    deriving (Eq, Ord)

makeLenses ''AST
makePrisms ''AST

instance Show (AST s) where
  show (AstModule _ i d) =
      "AstModule {ident=" <> show i
             <> ", decls=" <> show d
             <> "}"

  show (AstSignature _ i d) =
      "AstSignature {ident=" <> show i
                <> ", decls=" <> show d
                <> "}"

  show (AstAbstractType _ i t) =
      "AstAbstractType {ident=" <> show i
                   <> ", tyParams=" <> show t
                   <> "}"

  show (AstDataType _ i t cs) =
      "AstDataType {ident=" <> show i
               <> ", tyParams=" <> show t
               <> ", ctors=" <> show cs
               <> "}"

  show (AstRecordType _ i t cs) =
      "AstRecordType {ident=" <> show i
                 <> ", tyParams=" <> show t
                 <> ", fields=" <> show cs
                 <> "}"
