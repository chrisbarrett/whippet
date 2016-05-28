module Language.Whippet.Frontend.AST.HasIdentifier where

import           Control.Lens                         hiding (get, set)
import           Language.Whippet.Frontend.AST.Lenses
import           Language.Whippet.Frontend.AST.Types

typeIdentifiers :: Getter (Type s) (Maybe [Ident s])
typeIdentifiers =
    to get
  where
    get :: Type s -> Maybe [Ident s]
    get (TyNominal _ i _) = Just [i]
    get TyStructural {}   = Nothing
    get (TyFun _ xs) = fmap concat (mapM (view typeIdentifiers) xs)

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
        get (DecFun _ i _)          = i
        get (DecAbsType _ i _)      = i
        get (DecDataType _ i _ _)   = i
        get (DecRecordType _ i _ _) = i

        set :: Decl s -> Ident s -> Decl s
        set (DecFun p _ xs)         i    = DecFun p i xs
        set (DecAbsType p _ x)      i      = DecAbsType p i x
        set (DecDataType p _ x y)   i = DecDataType p i x y
        set (DecRecordType p _ x y) i = DecRecordType p i x y
