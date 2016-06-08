module Language.Whippet.Frontend.HasIdentifier where

import           Control.Lens                         hiding (get, set)
import qualified Data.List.NonEmpty                   as NonEmpty
import           Data.Monoid
import           Language.Whippet.Frontend.AST.Lenses
import           Language.Whippet.Frontend.AST.Types

class HasIdentifier a where
    identifier :: Getter a Ident

instance HasIdentifier ModuleId where
    identifier = _ModuleId.to NonEmpty.last

instance HasIdentifier Ident where
    identifier = id

instance HasIdentifier TypeParameter where
    identifier = typeParameterIdent

instance HasIdentifier Ctor where
    identifier = ctorIdent

instance HasIdentifier Field where
    identifier = fieldIdent

instance HasIdentifier Open where
    identifier = openId.identifier

instance HasIdentifier AST where
    identifier = to get
      where
        get :: AST -> Ident
        get (AstModule i _)    = i ^. identifier
        get (AstSignature i _) = i ^. identifier
        get (AstTypeclass i _) = i
        get (AstDecl d)        = d ^. identifier
        get (AstOpen o)        = o ^. identifier

instance HasIdentifier Decl where
    identifier = to get
      where
        get :: Decl -> Ident
        get (DecFun i _ _)        = i
        get (DecAbsType i _)      = i
        get (DecDataType i _ _)   = i
        get (DecRecordType i _ _) = i
