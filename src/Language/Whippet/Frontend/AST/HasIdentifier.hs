module Language.Whippet.Frontend.AST.HasIdentifier where

import           Control.Lens                         hiding (get, set)
import           Language.Whippet.Frontend.AST.Lenses
import           Language.Whippet.Frontend.AST.Types

typeIdentifier :: Lens' (Type s) (Maybe (Ident s))
typeIdentifier = undefined

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

        set :: AST s -> Ident s -> AST s
        set (AstModule p _ x)       i = AstModule p i x
        set (AstSignature p _ x)    i = AstSignature p i x
        set (AstAbstractType p _ x) i = AstAbstractType p i x
        set (AstDataType p _ x y)   i = AstDataType p i x y
