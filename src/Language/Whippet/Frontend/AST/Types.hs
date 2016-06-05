{-# LANGUAGE DeriveDataTypeable #-}
module Language.Whippet.Frontend.AST.Types where

import           Control.Lens.Plated
import           Data.Data
import           Data.Data.Lens      (uniplate)
import qualified Data.List           as List
import           Data.Monoid
import           Data.Scientific     (Scientific)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Text.Trifecta       as Trifecta

data AST
    = AstModule    Ident [AST]
    | AstSignature Ident [Decl]
    | AstDecl      Decl
    deriving (Eq, Ord, Show)

type Var = Ident

data Discriminator = DVar Var
    deriving (Eq, Ord, Show, Data)

data Pat = Pat Discriminator Expr
    deriving (Eq, Ord, Show, Data)

data Lit
    = LitList [Expr]
    | LitInt Integer
    | LitScientific Scientific
    | LitRecord [(Ident, Expr)]
    | LitString Text
    deriving (Eq, Ord, Show, Data)

data Expr
    = EAnnotation Expr Type
    | EApp Expr Expr
    | EHole Ident
    | EIf Expr Expr Expr
    | ELam [Pat]
    | ELet Pat (Maybe Type) Expr
    | ELit Lit
    | EMatch Expr [Pat]
    | EParen Expr
    | EVar Var
    deriving (Eq, Ord, Show, Data)

instance Plated Expr where
    plate = uniplate

data Ident = Ident {
      _identPos   :: Trifecta.Span
    , _identLabel :: Text
    }
    deriving (Ord, Show, Data)

instance Eq Ident where
  x == y = _identLabel x == _identLabel y

data Field = Field {
      _fieldIdent :: Ident
    , _fieldType  :: Type
    }
    deriving (Eq, Ord, Show, Data)

data Type
    = TyNominal    Ident
    | TyStructural [Field]
    | TyApp        Type Type
    | TyFun        Type Type
    deriving (Eq, Ord, Show, Data)

instance Plated Type where
    plate = uniplate

newtype TypeParameter = TypeParameter {_typeParameterIdent :: Ident}
    deriving (Eq, Ord, Show)

data Decl
    = DecFun        Ident Type (Maybe Expr)
    | DecAbsType    Ident [TypeParameter]
    | DecDataType   Ident [TypeParameter] [Ctor]
    | DecRecordType Ident [TypeParameter] [Field]
    deriving (Eq, Ord, Show)

data Ctor = Ctor {
      _ctorIdent  :: Ident
    , _ctorParams :: [Type]
    }
    deriving (Eq, Ord, Show)
