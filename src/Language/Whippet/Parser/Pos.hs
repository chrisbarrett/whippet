{-# LANGUAGE FlexibleInstances #-}
module Language.Whippet.Parser.Pos where

import           Control.Lens
import           Data.Int
import qualified Language.Whippet.Parser.Types as Parser

newtype Line = Line {_unLine :: Int64}
  deriving (Show, Eq, Ord)

newtype Col = Col {_unCol :: Int64}
  deriving (Show, Eq, Ord)

data Pos = Pos {
    _posLine   :: !Line
  , _posColumn :: !Col
  }
  deriving (Show, Eq, Ord)

emptyPos :: Pos
emptyPos = Pos (Line 0) (Col 0)

class HasPos a where
    position :: a -> Maybe Pos

instance HasPos (Parser.Type (Maybe Pos)) where
    position (Parser.TyNominal    p _) = p
    position (Parser.TyVar        p _) = p
    position (Parser.TyStructural p _) = p
    position (Parser.TyApp        p _ _) = p
    position (Parser.TyArrow      p _ _) = p
    position (Parser.TyForall     p _ _) = p
    position (Parser.TyConstraint p _ _) = p
