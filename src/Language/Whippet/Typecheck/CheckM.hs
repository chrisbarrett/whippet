{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Language.Whippet.Typecheck.CheckM where

import           Control.Lens
import           Control.Lens.TH
import qualified Control.Monad.State               as State
import           Data.Semigroup
import           Data.Sequence                     (Seq)
import qualified Data.Sequence                     as Seq
import qualified Data.Text                         as Text
import           Language.Whippet.Typecheck.Errors
import           Language.Whippet.Typecheck.Types

-- * Type-checking monad

-- |The type checking monad accumulates errors.
newtype CheckM a = CheckM {unCheckM :: State.State CheckState a}
    deriving (Functor, Applicative, Monad, State.MonadState CheckState)

-- |Type checker state tracking the current errors and the type variable
-- generator state.
data CheckState = CheckState {
      _checkStateErrs   :: Seq Err
    , _checkStateVarNum :: Int
    }

makeLenses ''CheckState
makePrisms ''CheckState

-- |The starting state for the type checker.
emptyCheckState :: CheckState
emptyCheckState = CheckState mempty 0

-- |Run the type checker monad, returning the inferred type and the final
-- checker state.
runCheckM :: CheckM a -> (a, CheckState)
runCheckM c = State.runState (unCheckM c) emptyCheckState

-- |Get a fresh type variable from the checker context.
freshTyVar :: CheckM Type
freshTyVar = do
    cur <- use checkStateVarNum
    checkStateVarNum += 1
    pure (TyVar ("t" <> Text.pack (show cur)))

-- |Add a type error to the current type checking context and mark a divergence.
failure :: Err -> CheckM Type
failure e = do
    checkStateErrs %= (|> e)
    pure TyDiverge

-- |A specialisation of 'pure' for symmetry with 'failure'.
success :: Type -> CheckM Type
success e =
    pure e
