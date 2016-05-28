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
      _fieldIdent :: Ident s
    , _fieldType  :: Type s
    }
    deriving (Eq, Ord)

fieldToText :: Field s -> Text
fieldToText Field {..} =
    ident <> ": " <> typeToText _fieldType
  where
    ident = _identLabel _fieldIdent

instance Show (Field s) where
  show = show . fieldToText

-- * Type Identifiers

data Type s
    = TyNominal    (Ident s)
    | TyStructural [Field s]
    | TyApp        (Type s) (Type s)
    | TyFun        (Type s) (Type s)
    deriving (Eq, Ord)

typeToText :: Type s -> Text

typeToText (TyNominal i ) =
    _identLabel i

typeToText (TyApp x y) =
    Text.unwords [typeToText x, typeToText y]

typeToText (TyStructural fs) =
    "{" <> Text.intercalate ", " (map fieldToText fs) <> "}"

typeToText (TyFun a b) =
    "(" <> typeToText a <> " -> " <> typeToText b <> ")"

instance Show (Type s) where
    show = show . typeToText

-- * Type Parameters

newtype TypeParameter s = TypeParameter {_typeParameterIdent :: Ident s}
    deriving (Eq, Ord)

instance Show (TypeParameter s) where
  show = show . _typeParameterIdent


-- * Declarations

data Decl s
    = DecFun        (Ident s) (Type s)
    | DecAbsType    (Ident s) [TypeParameter s]
    | DecDataType   (Ident s) [TypeParameter s] [Ctor s]
    | DecRecordType (Ident s) [TypeParameter s] [Field s]
    deriving (Eq, Ord)

instance Show (Decl s) where
  show (DecFun i t) =
      showRecord "DecFun"
          [ ("id", show i)
          , ("type", show t)
          ]
  show (DecAbsType i ps) =
      showRecord "DecAbsType"
          [ ("id", show i)
          , ("params", show ps)
          ]

  show (DecDataType i t cs) =
      showRecord "DecDataType"
          [ ("id", show i)
          , ("params", show t)
          , ("ctors", show cs)
          ]
  show (DecRecordType i t fs) =
      showRecord "DecRecordType"
          [ ("id", show i)
          , ("params", show t)
          , ("fields", show fs)
          ]

-- * Constructors

data Ctor s = Ctor {
      _ctorIdent  :: Ident s
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
          [("decl", show ds)]
