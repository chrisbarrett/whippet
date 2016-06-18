module Language.Whippet.Typecheck.Errors where

import           Language.Whippet.Typecheck.Types
import qualified Text.Trifecta                    as Trifecta

data Err = Err {
      _errLoc   :: Trifecta.Span
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
