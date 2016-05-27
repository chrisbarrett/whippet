module Language.Whippet.Frontend.AST.HasPos where

import           Control.Lens                         (Lens', lens)
import           Language.Whippet.Frontend.AST.Lenses
import           Language.Whippet.Frontend.AST.Types

class HasPos a where
    srcPos :: Lens' (a s) s

instance HasPos Ident where
    srcPos = identPos

instance HasPos Type where
    srcPos =
        lens get set
      where
        get :: Type s -> s
        get (TyNominal p _)    = p
        get (TyStructural p _) = p

        set :: Type s -> s -> Type s
        set (TyNominal _ x) p    = TyNominal p x
        set (TyStructural _ x) p = TyStructural p x

instance HasPos TypeParameter where
    srcPos = _TypeParameter.srcPos
