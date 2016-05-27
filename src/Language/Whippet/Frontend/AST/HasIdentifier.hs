module Language.Whippet.Frontend.AST.HasIdentifier where

import           Control.Lens                         hiding (get, set)
import           Language.Whippet.Frontend.AST.Lenses
import           Language.Whippet.Frontend.AST.Types

typeIdentifier :: Lens' (Type s) (Maybe (Ident s))
typeIdentifier =
    lens get set
  where
    get :: Type s -> Maybe (Ident s)
    get (TyNominal _ i) = Just i
    get TyStructural {} = Nothing

    set :: Type s -> Maybe (Ident s) -> Type s
    set t@(TyNominal p _) i = maybe t (TyNominal p) i
    set t@TyStructural {} _ = t

class HasIdentifier a where
    identifier :: Lens' (a s) (Ident s)

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
        get :: AST s -> Ident s
        get (AstModule i _)    = i
        get (AstSignature i _) = i
        get (AstDecl d)        = d^.identifier

        set :: AST s -> Ident s -> AST s
        set (AstModule _ x)    i = AstModule i x
        set (AstSignature _ x) i = AstSignature i x
        set (AstDecl d)        i = AstDecl (d & identifier .~ i)

instance HasIdentifier Decl where
    identifier =
        lens get set
      where
        get :: Decl s -> Ident s
        get (DecFn _ i _)       = i
        get (DecAbsType _ i _) = i
        get (DecDataType _ i _ _)   = i
        get (DecRecordType _ i _ _) = i

        set :: Decl s -> Ident s -> Decl s
        set (DecFn p _ xs)       i    = DecFn p i xs
        set (DecAbsType p _ x) i      = DecAbsType p i x
        set (DecDataType p _ x y)   i = DecDataType p i x y
        set (DecRecordType p _ x y) i = DecRecordType p i x y
