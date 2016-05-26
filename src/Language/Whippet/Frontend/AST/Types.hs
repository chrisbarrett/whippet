{-# LANGUAGE NamedFieldPuns #-}
module Language.Whippet.Frontend.AST.Types where

import           Data.Monoid
import           Data.Text   (Text)

-- * Identifiers

data Ident s = Ident {
      _identPos   :: s
    , _identLabel :: Text
    }
    deriving (Eq, Ord)

instance Show (Ident s) where
  show =  show . _identLabel


-- * Record Fields

data Field s = Field {
      _fieldSpan  :: s
    , _fieldIdent :: Ident s
    , _fieldType  :: Type s
    }
    deriving (Eq, Ord)

instance Show (Field s) where
  show Field {_fieldIdent, _fieldType} =
      "Field {ident=" <> show _fieldIdent <> ", type=" <> show _fieldType <> "}"


-- * Type Identifiers

data Type s
    = TyNominal    s (Ident s)
    | TyStructural s [Field s]
    deriving (Eq, Ord)

instance Show (Type s) where
    show (TyNominal _ i)     = show i
    show (TyStructural _ fs) = show fs


-- * Type Parameters

newtype TypeParameter s = TypeParameter {_typeParameterIdent :: Ident s}
    deriving (Eq, Ord)

instance Show (TypeParameter s) where
  show = show . _typeParameterIdent


-- * Declarations

data Decl s = FnDecl s (Ident s) [Type s]
    deriving (Eq, Ord)

instance Show (Decl s) where
  show _ = ""


-- * Constructors

data Ctor s = Ctor {
      _ctorSpan   :: s
    , _ctorIdent  :: Ident s
    , _ctorParams :: [Type s]
    }
    deriving (Eq, Ord)

instance Show (Ctor s) where
  show Ctor {_ctorIdent, _ctorParams} =
      "Ctor {ident=" <> show _ctorIdent <> ", params=" <> show _ctorParams <> "}"


-- * Top-level declarations

data AST s
    = AstModule       s (Ident s) [Decl s]
    | AstSignature    s (Ident s) [Decl s]
    | AstAbstractType s (Ident s) [TypeParameter s]
    | AstDataType     s (Ident s) [TypeParameter s] [Ctor s]
    | AstRecordType   s (Ident s) [TypeParameter s] [Field s]
    deriving (Eq, Ord)

instance Show (AST s) where
  show (AstModule _ i d) =
      "AstModule {ident=" <> show i <> ", decls=" <> show d <> "}"
  show (AstSignature _ i d) =
      "AstSignature {ident=" <> show i <> ", decls=" <> show d <> "}"
  show (AstAbstractType _ i t) =
      "AstAbstractType {ident=" <> show i <> ", tyParams=" <> show t <> "}"
  show (AstDataType _ i t cs) =
      "AstDataType {ident=" <> show i
               <> ", tyParams=" <> show t
               <> ", ctors=" <> show cs
               <> "}"
  show (AstRecordType _ i t fs) =
      "AstRecordType {ident=" <> show i
               <> ", tyParams=" <> show t
               <> ", fields=" <> show fs
               <> "}"
