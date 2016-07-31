module Language.Whippet.Typecheck.Errors where

import           Language.Whippet.Parser.Pos
import           Language.Whippet.Typecheck.Types

data Err = Err {
      _errLoc   :: Pos
    , _errError :: Error
    }
    deriving (Show, Eq, Ord)

data Error = ErrorUnification UnificationError
    deriving (Show, Eq, Ord)

data UnificationError = UnificationError {
      _unificationErrorExpected :: Type
    , _unificationErrorActual   :: Type
    }
    deriving (Show, Eq, Ord)
