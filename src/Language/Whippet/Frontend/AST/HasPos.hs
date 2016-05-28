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
        get (TyNominal p _ _)  = p
        get (TyStructural p _) = p
        get (TyFun p _ _)      = p

        set :: Type s -> s -> Type s
        set (TyNominal _ x xs) p = TyNominal p x xs
        set (TyStructural _ x) p = TyStructural p x
        set (TyFun _ a b)       p = TyFun p a b

instance HasPos TypeParameter where
    srcPos = _TypeParameter.srcPos
