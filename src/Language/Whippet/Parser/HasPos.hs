{-# LANGUAGE FlexibleInstances #-}
module Language.Whippet.Parser.HasPos where

import           Control.Lens
import           Language.Whippet.Parser.Lenses
import qualified Language.Whippet.Parser.Types  as Parser
import qualified Text.Trifecta                  as Trifecta

type Pos = Trifecta.Span

emptyPos :: Pos
emptyPos = Trifecta.Span mempty mempty mempty

class HasPos a where
    position :: a -> Maybe Pos

instance HasPos (Parser.Type Pos) where
    position (Parser.TyNominal    p _) = p
    position (Parser.TyVar        p _) = p
    position (Parser.TyStructural p _) = p
    position (Parser.TyApp        p _ _) = p
    position (Parser.TyArrow      p _ _) = p
    position (Parser.TyForall     p _ _) = p
    position (Parser.TyConstraint p _ _) = p
