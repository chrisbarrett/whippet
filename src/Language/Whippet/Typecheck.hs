module Language.Whippet.Typecheck (
      module M
    , Checkable (..)
    ) where

import           Data.Validation
import           Language.Whippet.Typecheck.Errors as M
import           Language.Whippet.Typecheck.Lenses as M

class Checkable a where
    check :: a -> AccValidation [Err] a
