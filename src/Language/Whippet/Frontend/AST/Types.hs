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
    = TyNominal    s (Ident s) [Ident s]
    | TyStructural s [Field s]
    | TyFun        s (Type s) (Type s)
    deriving (Eq, Ord)

tyToText :: Type s -> Text

tyToText (TyNominal _ i ps) = Text.unwords (map _identLabel (i : ps))

tyToText (TyStructural _ fs) = Text.pack (show fs)

tyToText (TyFun _ a b) =
    "(" <> tyToText a <> " -> " <> tyToText b <> ")"

instance Show (Type s) where
    show = show . tyToText

-- * Type Parameters

newtype TypeParameter s = TypeParameter {_typeParameterIdent :: Ident s}
    deriving (Eq, Ord)

instance Show (TypeParameter s) where
  show = show . _typeParameterIdent


-- * Declarations

data Decl s
    = DecFun        s (Ident s) [Type s]
    | DecAbsType    s (Ident s) [TypeParameter s]
    | DecDataType   s (Ident s) [TypeParameter s] [Ctor s]
    | DecRecordType s (Ident s) [TypeParameter s] [Field s]
    deriving (Eq, Ord)

instance Show (Decl s) where
  show (DecFun _ i ts) =
      showRecord "DecFun"
          [ ("id", show i)
          , ("types", show ts)
          ]
  show (DecAbsType _ i ps) =
      showRecord "DecAbsType"
          [ ("id", show i)
          , ("params", show ps)
          ]

  show (DecDataType _ i t cs) =
      showRecord "DecDataType"
          [ ("id", show i)
          , ("params", show t)
          , ("ctors", show cs)
          ]
  show (DecRecordType _ i t fs) =
      showRecord "DecRecordType"
          [ ("id", show i)
          , ("params", show t)
          , ("fields", show fs)
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
    = AstModule    (Ident s) [AST s]
    | AstSignature (Ident s) [Decl s]
    | AstDecl      (Decl s)
    deriving (Eq, Ord)

instance Show (AST s) where
  show (AstModule i ds) =
      showRecord "AstModule"
          [ ("id", show i)
          , ("decls", show ds)
          ]
  show (AstSignature i ds) =
      showRecord "AstSignature"
          [ ("id", show i)
          , ("decls", show ds)
          ]
  show (AstDecl ds) =
      showRecord "AstDecl"
          [("decls", show ds)]
