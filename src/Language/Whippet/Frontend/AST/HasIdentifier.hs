module Language.Whippet.Frontend.AST.HasIdentifier where

import           Control.Lens                         hiding (get, set)
import           Data.Monoid
import           Language.Whippet.Frontend.AST.Lenses
import           Language.Whippet.Frontend.AST.Types

typeIdentifiers :: Type -> Maybe [Ident]

typeIdentifiers (TyNominal i)   = Just [i]
typeIdentifiers (TyVar i)       = Just [i]
typeIdentifiers TyStructural {} = Nothing

typeIdentifiers (TyApp x y) =
    concat <$> sequence [typeIdentifiers x, typeIdentifiers y]

typeIdentifiers (TyFun a b) = do
    as <- typeIdentifiers a
    bs <- typeIdentifiers b
    pure (as <> bs)

class HasIdentifier a where
    identifier :: Getter a Ident

instance HasIdentifier Ident where
    identifier = id

instance HasIdentifier TypeParameter where
    identifier = typeParameterIdent

instance HasIdentifier Ctor where
    identifier = ctorIdent

instance HasIdentifier Field where
    identifier = fieldIdent

instance HasIdentifier Open where
    identifier = openIdent

instance HasIdentifier AST where
    identifier = to get
      where
        get :: AST -> Ident
        get (AstModule i _)    = i
        get (AstSignature i _) = i
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
