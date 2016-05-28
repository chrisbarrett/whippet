module Language.Whippet.Frontend.AST.HasIdentifier where

import           Control.Lens                         hiding (get, set)
import           Data.Monoid
import           Language.Whippet.Frontend.AST.Lenses
import           Language.Whippet.Frontend.AST.Types

typeIdentifiers :: Type s -> Maybe [Ident s]

typeIdentifiers (TyNominal i)   = Just [i]
typeIdentifiers TyStructural {} = Nothing

typeIdentifiers (TyApp x y) =
    concat <$> sequence [typeIdentifiers x, typeIdentifiers y]

typeIdentifiers (TyFun a b) = do
    as <- typeIdentifiers a
    bs <- typeIdentifiers b
    pure (as <> bs)

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
        get (DecFun i _)          = i
        get (DecAbsType i _)      = i
        get (DecDataType i _ _)   = i
        get (DecRecordType i _ _) = i

        set :: Decl s -> Ident s -> Decl s
        set (DecFun _ xs)         i = DecFun i xs
        set (DecAbsType _ x)      i = DecAbsType i x
        set (DecDataType _ x y)   i = DecDataType i x y
        set (DecRecordType _ x y) i = DecRecordType i x y
