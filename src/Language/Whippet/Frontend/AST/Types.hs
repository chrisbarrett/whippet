{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Language.Whippet.Frontend.AST.Types where

import qualified Data.List   as List
import           Data.Monoid
import           Data.Text   (Text)
import qualified Data.Text   as Text

showRecord :: Text -> [(Text, String)] -> String
showRecord ty fields =
    Text.unpack ty <> "{" <> render fields <> "}"
  where
    render = List.intercalate ", " . fmap renderField
    renderField (l, x) = Text.unpack l <> "=" <> x

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
  show Field {..} =
      showRecord "Field"
          [ ("id", show _fieldIdent)
          , ("type", show _fieldType)
          ]


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
  show (FnDecl _ i ts) =
      showRecord "FnDecl"
          [ ("id", show i)
          , ("types", show ts)
          ]


-- * Constructors

data Ctor s = Ctor {
      _ctorSpan   :: s
    , _ctorIdent  :: Ident s
    , _ctorParams :: [Type s]
    }
    deriving (Eq, Ord)

instance Show (Ctor s) where
  show Ctor {..} =
      showRecord "Ctor"
          [ ("id", show _ctorIdent)
          , ("params", show _ctorParams)
          ]


-- * Top-level declarations

data AST s
    = AstModule       s (Ident s) [Decl s]
    | AstSignature    s (Ident s) [Decl s]
    | AstAbstractType s (Ident s) [TypeParameter s]
    | AstDataType     s (Ident s) [TypeParameter s] [Ctor s]
    | AstRecordType   s (Ident s) [TypeParameter s] [Field s]
    deriving (Eq, Ord)

instance Show (AST s) where
  show (AstModule _ i ds) =
      showRecord "AstModule"
          [ ("id", show i)
          , ("decls", show ds)
          ]
  show (AstSignature _ i ds) =
      showRecord "AstSignature"
          [ ("id", show i)
          , ("decls", show ds)
          ]
  show (AstAbstractType _ i t) =
      showRecord "AstAbstractType"
          [ ("id", show i)
          , ("params", show t)
          ]
  show (AstDataType _ i t cs) =
      showRecord "AstDataType"
          [ ("id", show i)
          , ("params", show t)
          , ("ctors", show cs)
          ]
  show (AstRecordType _ i t fs) =
      showRecord "AstRecordType"
          [ ("id", show i)
          , ("params", show t)
          , ("fields", show fs)
          ]
