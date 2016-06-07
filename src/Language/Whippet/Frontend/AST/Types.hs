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

data Discriminator
    = DVar Ident
    | DWildcard Ident
    | DCtor Ident
    | DAnn Discriminator Type
    | DRec [Discriminator]
    | DAs Discriminator Discriminator
    | DApp Discriminator Discriminator
    deriving (Eq, Ord, Show, Data)

data Pat = Pat {
      _patDiscriminator :: Discriminator
    , _patBody          :: Expr
    }
    deriving (Eq, Ord, Show, Data)

data Lit
    = LitList [Expr]
    | LitInt Integer
    | LitChar Char
    | LitScientific Scientific
    | LitRecord [(Ident, Expr)]
    | LitString Text
    deriving (Eq, Ord, Show, Data)

data Expr
    = EAnnotation Expr Type
    | EApp Expr Expr
    | EHole Ident
    | EIf Expr Expr Expr
    | EFn [Pat]
    | ELet Discriminator Expr Expr
    | ELit Lit
    | EMatch Expr [Pat]
    | EVar Ident
    deriving (Eq, Ord, Show, Data)

instance Plated Expr where
    plate = uniplate

data Ident = Ident {
      _identPos   :: Trifecta.Span
    , _identLabel :: Text
    }
    deriving (Ord, Data)

instance Show Ident where
    show = show . _identLabel

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
