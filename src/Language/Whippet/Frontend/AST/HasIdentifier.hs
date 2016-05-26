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
        get (AstModule _ i _)       = i
        get (AstSignature _ i _)    = i
        get (AstAbstractType _ i _) = i
        get (AstDataType _ i _ _)   = i
        get (AstRecordType _ i _ _) = i

        set :: AST s -> Ident s -> AST s
        set (AstModule p _ x)       i = AstModule p i x
        set (AstSignature p _ x)    i = AstSignature p i x
        set (AstAbstractType p _ x) i = AstAbstractType p i x
        set (AstDataType p _ x y)   i = AstDataType p i x y
        set (AstRecordType p _ x y) i = AstRecordType p i x y

instance HasIdentifier Decl where
    identifier =
        lens get set
      where
        get :: Decl s -> Ident s
        get (FnDecl _ i _)       = i

        set :: Decl s -> Ident s -> Decl s
        set (FnDecl p _ xs)       i = FnDecl p i xs
