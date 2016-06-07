module Language.Whippet.Frontend.AST.HasIdentifier where

import           Control.Lens                         hiding (get, set)
import           Data.Monoid
import           Language.Whippet.Frontend.AST.Lenses
import           Language.Whippet.Frontend.AST.Types

typeIdentifiers :: Type -> Maybe [Ident]

typeIdentifiers (TyNominal i)   = Just [i]
typeIdentifiers TyStructural {} = Nothing

typeIdentifiers (TyApp x y) =
    concat <$> sequence [typeIdentifiers x, typeIdentifiers y]

typeIdentifiers (TyFun a b) = do
    as <- typeIdentifiers a
    bs <- typeIdentifiers b
    pure (as <> bs)

class HasIdentifier a where
    identifier :: Lens' a Ident

instance HasIdentifier Ident where
    identifier = id

instance HasIdentifier TypeParameter where
    identifier = typeParameterIdent

instance HasIdentifier Ctor where
    identifier = ctorIdent

instance HasIdentifier Field where
    identifier = fieldIdent

instance HasIdentifier AST where
    identifier =
        lens get set
      where
        get :: AST -> Ident
        get (AstModule i _)    = i
        get (AstSignature i _) = i
        get (AstTypeclass i _) = i
        get (AstDecl d)        = d^.identifier

        set :: AST -> Ident -> AST
        set (AstModule _ x)    i = AstModule i x
        set (AstSignature _ x) i = AstSignature i x
        set (AstTypeclass _ x) i = AstTypeclass i x
        set (AstDecl d)        i = AstDecl (d & identifier .~ i)

instance HasIdentifier Decl where
    identifier =
        lens get set
      where
        get :: Decl -> Ident
        get (DecFun i _ _)        = i
        get (DecAbsType i _)      = i
        get (DecDataType i _ _)   = i
        get (DecRecordType i _ _) = i

        set :: Decl -> Ident -> Decl
        set (DecFun _ xs e)       i = DecFun i xs e
        set (DecAbsType _ x)      i = DecAbsType i x
        set (DecDataType _ x y)   i = DecDataType i x y
        set (DecRecordType _ x y) i = DecRecordType i x y
