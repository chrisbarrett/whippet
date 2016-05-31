{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Language.Whippet.Frontend.AST.Types where

import qualified Data.List       as List
import           Data.Monoid
import           Data.Scientific (Scientific)
import           Data.Text       (Text)
import qualified Data.Text       as Text
import qualified Text.Trifecta   as Trifecta

showRecord :: Text -> [(Text, String)] -> String
showRecord ty fields =
    Text.unpack ty <> "{" <> render fields <> "}"
  where
    render = List.intercalate ", " . fmap renderField
    renderField (l, x) = Text.unpack l <> "=" <> x

-- * Identifiers

data Ident = Ident {
      _identPos   :: Trifecta.Span
    , _identLabel :: Text
    }
    deriving (Eq, Ord)

instance Show Ident where
  show =  show . _identLabel


-- * Record Fields

data Field = Field {
      _fieldIdent :: Ident
    , _fieldType  :: Type
    }
    deriving (Eq, Ord)

fieldToText :: Field -> Text
fieldToText Field {..} =
    ident <> ": " <> typeToText _fieldType
  where
    ident = _identLabel _fieldIdent

instance Show Field where
  show = show . fieldToText

-- * Type Identifiers

data Type
    = TyNominal    Ident
    | TyStructural [Field]
    | TyApp        Type Type
    | TyFun        Type Type
    deriving (Eq, Ord)

typeToText :: Type -> Text

typeToText (TyNominal i ) =
    _identLabel i

typeToText (TyApp x y) =
    Text.unwords [typeToText x, typeToText y]

typeToText (TyStructural fs) =
    "{" <> Text.intercalate ", " (map fieldToText fs) <> "}"

typeToText (TyFun a b) =
    "(" <> typeToText a <> " -> " <> typeToText b <> ")"

instance Show Type where
    show = show . typeToText

-- * Type Parameters

newtype TypeParameter = TypeParameter {_typeParameterIdent :: Ident}
    deriving (Eq, Ord)

instance Show TypeParameter where
  show = show . _typeParameterIdent


-- * Declarations

data Decl
    = DecFun        Ident Type (Maybe Expr)
    | DecAbsType    Ident [TypeParameter]
    | DecDataType   Ident [TypeParameter] [Ctor]
    | DecRecordType Ident [TypeParameter] [Field]
    deriving (Eq, Ord)

instance Show Decl where
  show (DecFun i t e) =
      showRecord "DecFun"
          [ ("id", show i)
          , ("type", show t)
          , ("expr", show e)
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

data Ctor = Ctor {
      _ctorIdent  :: Ident
    , _ctorParams :: [Type]
    }
    deriving (Eq, Ord)

instance Show Ctor where
  show Ctor {..} =
      showRecord "Ctor"
          [ ("id", show _ctorIdent)
          , ("params", show _ctorParams)
          ]


-- * Top-level declarations

data AST
    = AstModule    Ident [AST]
    | AstSignature Ident [Decl]
    | AstDecl      Decl
    deriving (Eq, Ord)

instance Show AST where
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

-- * Expressions

type Var = Ident

data Discriminator = DiscVar Var
    deriving (Eq, Ord, Show)

data Pat = Pat Discriminator Expr
    deriving (Eq, Ord, Show)

data Lit
    = LitList [Expr]
    | LitInt Integer
    | LitScientific Scientific
    | LitRecord [(Ident, Expr)]
    | LitString Text
    deriving (Eq, Ord, Show)

data Expr
    = EAnnotate Expr Type
    | EApp Expr Expr
    | EHole
    | EIf Expr Expr Expr
    | ELam [Pat]
    | ELet Pat (Maybe Type) Expr
    | ELit Lit
    | EMatch Expr [Pat]
    | EParen Expr
    | EVar Var
    deriving (Eq, Ord, Show)
