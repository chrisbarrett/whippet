{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Language.Whippet.Frontend.AST.Types where

import           Control.Lens.Plated
import           Data.Data
import           Data.Data.Lens      (uniplate)
import qualified Data.List           as List
import           Data.List.NonEmpty  (NonEmpty)
import qualified Data.List.NonEmpty  as NonEmpty
import           Data.Monoid
import           Data.Scientific     (Scientific)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Text.Trifecta       as Trifecta

data AST
    = AstModule    ModuleId [AST]
    | AstSignature ModuleId [Decl]
    | AstDecl      Decl
    | AstTypeclass Ident [Decl]
    | AstOpen      Open
    deriving (Eq, Ord, Show)

data Open = Open {
      _openId     :: ModuleId
    , _openAs     :: Maybe Ident
    , _openHiding :: Maybe [Ident]
    }
    deriving (Eq, Ord, Show, Data)

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
    | EOpen Open Expr
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
    | TyVar        Ident
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

data ModuleId = ModuleId (NonEmpty Ident)
    deriving (Eq, Ord, Show, Data)
