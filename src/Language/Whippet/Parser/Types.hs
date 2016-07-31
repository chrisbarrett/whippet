{-# LANGUAGE DeriveFunctor #-}
module Language.Whippet.Parser.Types where

import qualified Data.List          as List
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Monoid
import           Data.Scientific    (Scientific)
import           Data.Text          (Text)
import qualified Data.Text          as Text

data AST a
    = AstModule    (Module a)
    | AstSignature (Signature a)
    | AstDecl      (Decl a)
    | AstOpen      (Open a)
    deriving (Functor, Eq, Ord, Show)

data Module a = Module {
      _moduleId   :: QualId a
    , _moduleBody :: [AST a]
    }
    deriving (Functor, Eq, Ord, Show)

data Signature a = Signature {
      _signatureId   :: QualId a
    , _signatureBody :: [Decl a]
    }
    deriving (Functor, Eq, Ord, Show)

data Open a = Open {
      _openId     :: QualId a
    , _openAs     :: Maybe (Ident a)
    , _openHiding :: Maybe [Ident a]
    }
    deriving (Functor, Eq, Ord, Show)

data Discriminator a
    = DVar (Ident a)
    | DWildcard (Ident a)
    | DCtor (Ident a)
    | DAnn (Discriminator a) (Type a)
    | DRec [Discriminator a]
    | DAs (Discriminator a) (Discriminator a)
    | DApp (Discriminator a) (Discriminator a)
    deriving (Functor, Eq, Ord, Show)

data Pat a = Pat {
      _patDiscriminator :: Discriminator a
    , _patGuard         :: Maybe (Guard a)
    , _patBody          :: Expr a
    }
    deriving (Functor, Eq, Ord, Show)

data Guard a
    = IfGuard (Expr a)
    | UnlessGuard (Expr a)
    deriving (Functor, Eq, Ord, Show)

data Lit a
    = LitList [Expr a]
    | LitInt Integer
    | LitChar Char
    | LitScientific Scientific
    | LitRecord [(Ident a, Expr a)]
    | LitString Text
    deriving (Functor, Eq, Ord, Show)

data Expr a
    = EAnnotation (Annotation a)
    | EApp (App a)
    | EHole (Ident a)
    | EIf (If a)
    | EFn [Pat a]
    | ELet (Let a)
    | ELit (Lit a)
    | EMatch (Match a)
    | EVar (Ident a)
    | EOpen (Open a) (Expr a)
    deriving (Functor, Eq, Ord, Show)

data App a = App {
      _appFn  :: Expr a
    , _appArg :: Expr a
    }
    deriving (Functor, Eq, Ord, Show)

data If a = If {
      _ifCondition :: Expr a
    , _ifThen      :: Expr a
    , _ifElse      :: Expr a
    }
    deriving (Functor, Eq, Ord, Show)

data Let a = Let {
      _letDiscriminator :: Discriminator a
    , _letValue         :: Expr a
    , _letBody          :: Expr a
    }
    deriving (Functor, Eq, Ord, Show)

data Match a = Match {
      _matchExpr     :: Expr a
    , _matchPatterns :: [Pat a]
    }
    deriving (Functor, Eq, Ord, Show)

data Annotation a = Annotation {
      _annotationPos  :: a
    , _annotationExpr :: Expr a
    , _annotationType :: Type a
    }
    deriving (Functor, Eq, Ord, Show)

data Ident a = Ident {
      _identPos   :: a
    , _identLabel :: Text
    }
    deriving (Functor, Ord)

instance Show (Ident a) where
    show = show . _identLabel

instance Eq (Ident a) where
    x == y = _identLabel x == _identLabel y

data Field a = Field {
      _fieldIdent :: Ident a
    , _fieldType  :: Type a
    }
    deriving (Functor, Eq, Ord, Show)

data Type a
    = TyNominal    a (QualId a)
    | TyVar        a (Ident a)
    | TyStructural a [Field a]
    | TyApp        a (Type a) (Type a)
    | TyArrow      a (Type a) (Type a)
    | TyForall     a (NonEmpty (TypeParameter a)) (Type a)
    | TyConstraint a (NonEmpty (Constraint a)) (Type a)
    deriving (Functor, Eq, Ord, Show)

data Constraint a = Constraint {
      _constraintCtor   :: Ident a
    , _constraintParams :: NonEmpty (TypeParameter a)
    }
    deriving (Functor, Eq, Ord, Show)

newtype TypeParameter a = TypeParameter {_typeParameterIdent :: Ident a}
    deriving (Functor, Eq, Ord, Show)

data Decl a
    = DecFun        (Function a)
    | DecFunSig     (FunctionSig a)
    | DecAbsType    (AbsType a)
    | DecDataType   (DataType a)
    | DecRecordType (RecordType a)
    | DecTypeclass  (Typeclass a)
    | DecInstance   (Instance a)
    deriving (Functor, Eq, Ord, Show)

data FnOrSig a = Fn (Function a) | Sig (FunctionSig a)
    deriving (Functor, Eq, Ord, Show)

data Typeclass a = Typeclass {
      _typeclassIdent :: Ident a
    , _typeclassDecls :: [FnOrSig a]
    }
    deriving (Functor, Eq, Ord, Show)

data Instance a = Instance {
      _instanceIdent  :: QualId a
    , _instanceTarget :: Type a
    , _instanceDecls  :: [Function a]
    }
    deriving (Functor, Eq, Ord, Show)

data Function a = Function {
      _functionIdent  :: Ident a
    , _functionParams :: [FnParam a]
    , _functionType   :: Maybe (Type a)
    , _functionBody   :: Expr a
    }
    deriving (Functor, Eq, Ord, Show)

data FunctionSig a = FunctionSig {
      _functionSigIdent :: Ident a
    , _functionSigType  :: Type a
    }
    deriving (Functor, Eq, Ord, Show)

data AbsType a = AbsType {
      _absTypeIdent  :: Ident a
    , _absTypeParams :: [TypeParameter a]
    }
    deriving (Functor, Eq, Ord, Show)

data DataType a = DataType {
      _dataTypeIdent  :: Ident a
    , _dataTypeParams :: [TypeParameter a]
    , _dataTypeCtors  :: [Ctor a]
    }
    deriving (Functor, Eq, Ord, Show)

data RecordType a = RecordType {
      _recordTypeIdent  :: Ident a
    , _recordTypeParams :: [TypeParameter a]
    , _recordTypeFields :: [Field a]
    }
    deriving (Functor, Eq, Ord, Show)

data FnParam a = FnParam {
      _fnParamIdent :: Ident a
    , _fnParamType  :: Maybe (Type a)
    }
    deriving (Functor, Eq, Ord, Show)

data Ctor a = Ctor {
      _ctorIdent  :: Ident a
    , _ctorParams :: [Type a]
    }
    deriving (Functor, Eq, Ord, Show)

data QualId a = QualId (NonEmpty (Ident a))
    deriving (Functor, Eq, Ord, Show)
