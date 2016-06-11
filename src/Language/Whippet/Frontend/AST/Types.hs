{-# LANGUAGE DeriveDataTypeable #-}
module Language.Whippet.Frontend.AST.Types where

import           Control.Lens.Plated
import           Data.Data           (Data)
import           Data.Data.Lens      (uniplate)
import qualified Data.List           as List
import           Data.List.NonEmpty  (NonEmpty)
import qualified Data.List.NonEmpty  as NonEmpty
import           Data.Monoid
import           Data.Scientific     (Scientific)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Text.Trifecta       as Trifecta

type TopLevel = [AST]

data AST
    = AstModule    Module
    | AstSignature Signature
    | AstDecl      Decl
    | AstOpen      Open
    deriving (Eq, Ord, Show, Data)

data Module = Module {
      _moduleId   :: QualId
    , _moduleBody :: [AST]
    }
    deriving (Eq, Ord, Show, Data)

data Signature = Signature {
      _signatureId   :: QualId
    , _signatureBody :: [Decl]
    }
    deriving (Eq, Ord, Show, Data)

data Open = Open {
      _openId     :: QualId
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
    = EAnnotation Annotation
    | EApp App
    | EHole Ident
    | EIf If
    | EFn [Pat]
    | ELet Let
    | ELit Lit
    | EMatch Match
    | EVar Ident
    | EOpen Open Expr
    deriving (Eq, Ord, Show, Data)

instance Plated Expr where
    plate = uniplate

data App = App {
      _appFn  :: Expr
    , _appArg :: Expr
    }
    deriving (Eq, Ord, Show, Data)

data If = If {
      _ifCondition :: Expr
    , _ifThen      :: Expr
    , _ifElse      :: Expr
    }
    deriving (Eq, Ord, Show, Data)

data Let = Let {
      _letDiscriminator :: Discriminator
    , _letValue         :: Expr
    , _letBody          :: Expr
    }
    deriving (Eq, Ord, Show, Data)

data Match = Match {
      _matchExpr     :: Expr
    , _matchPatterns :: [Pat]
    }
    deriving (Eq, Ord, Show, Data)

data Annotation = Annotation {
      _annotationExpr :: Expr
    , _annotationType :: Type
    }
    deriving (Eq, Ord, Show, Data)

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
    = TyNominal    QualId
    | TyVar        Ident
    | TyStructural [Field]
    | TyApp        Type Type
    | TyArrow      Type Type
    deriving (Eq, Ord, Show, Data)

instance Plated Type where
    plate = uniplate

newtype TypeParameter = TypeParameter {_typeParameterIdent :: Ident}
    deriving (Eq, Ord, Show, Data)

data Decl
    = DecFun        Function
    | DecFunSig     FunctionSig
    | DecAbsType    AbsType
    | DecDataType   DataType
    | DecRecordType RecordType
    | DecTypeclass  Typeclass
    | DecInstance   Instance
    deriving (Eq, Ord, Show, Data)

data FnOrSig = Fn Function | Sig FunctionSig
    deriving (Eq, Ord, Show, Data)

data Typeclass = Typeclass {
      _typeclassIdent :: Ident
    , _typeclassDecls :: [FnOrSig]
    }
    deriving (Eq, Ord, Show, Data)

data Instance = Instance {
      _instanceIdent  :: QualId
    , _instanceTarget :: Type
    , _instanceDecls  :: [Function]
    }
    deriving (Eq, Ord, Show, Data)

data Function = Function {
      _functionIdent  :: Ident
    , _functionParams :: [FnParam]
    , _functionType   :: Maybe Type
    , _functionBody   :: Expr
    }
    deriving (Eq, Ord, Show, Data)

data FunctionSig = FunctionSig {
      _functionSigIdent :: Ident
    , _functionSigType  :: Type
    }
    deriving (Eq, Ord, Show, Data)

data AbsType = AbsType {
      _absTypeIdent  :: Ident
    , _absTypeParams :: [TypeParameter]
    }
    deriving (Eq, Ord, Show, Data)

data DataType = DataType {
      _dataTypeIdent  :: Ident
    , _dataTypeParams :: [TypeParameter]
    , _dataTypeCtors  :: [Ctor]
    }
    deriving (Eq, Ord, Show, Data)

data RecordType = RecordType {
      _recordTypeIdent  :: Ident
    , _recordTypeParams :: [TypeParameter]
    , _recordTypeFields :: [Field]
    }
    deriving (Eq, Ord, Show, Data)

data FnParam = FnParam {
      _fnParamIdent :: Ident
    , _fnParamType  :: Maybe Type
    }
    deriving (Eq, Ord, Show, Data)

data Ctor = Ctor {
      _ctorIdent  :: Ident
    , _ctorParams :: [Type]
    }
    deriving (Eq, Ord, Show, Data)

data QualId = QualId (NonEmpty Ident)
    deriving (Eq, Ord, Show, Data)
