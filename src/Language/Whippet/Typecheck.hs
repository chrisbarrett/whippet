{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
module Language.Whippet.Typecheck (
      module Language.Whippet.Typecheck.Errors
    , module Language.Whippet.Typecheck.Lenses
    , module Language.Whippet.Typecheck.Types
    , Checkable (..)
    , TypeCheck
    , Check
    , toEither
    ) where

import           Control.Lens
import qualified Control.Monad.State               as State
import           Data.Sequence                     (Seq)
import qualified Data.Sequence                     as Seq
import           Language.Whippet.Parser.HasPos
import           Language.Whippet.Parser.Lenses
import qualified Language.Whippet.Parser.Types     as Parser
import           Language.Whippet.Typecheck.Errors
import           Language.Whippet.Typecheck.Lenses
import           Language.Whippet.Typecheck.Types
import qualified Text.Trifecta                     as Trifecta

-- * Type-checking monad

-- |The type checking monad accumulates errors.
newtype Check a = Check {unCheck :: State.State (Seq Err) a}
    deriving (Functor, Applicative, Monad, State.MonadState (Seq Err))

type TypeCheck = Check Type

-- |Convert a type checking result to an 'Either'. Return 'Right' if there were
-- no errors, otherwise left.
toEither :: Check a -> Either (Seq Err) a
toEither (Check c) =
    let (t, errs) = State.runState c mempty
    in
      if Seq.null errs
        then Right t
        else Left errs

-- |Add a type error to the current type checking context and mark a divergence.
failure :: Err -> TypeCheck
failure e = do
    State.modify (|> e)
    pure TyDiverge

resolve :: Parser.Type a -> TypeCheck
resolve = undefined

-- |Return the first of the two types if they unify, otherwise add a type error
-- to the type checking context and return a divergence marker.
unify :: Trifecta.Span -> Type -> Type -> TypeCheck
unify p t1 t2 =
    if t1 == t2
      then
        pure t1
      else
        unificationError p t1 t2

  where
    unificationError :: Trifecta.Span -> Type -> Type -> TypeCheck
    unificationError s t1 t2 = failure (Err s e)
        where
          e = ErrorUnification (UnificationError t1 t2)

-- * Type-checker

-- |The 'Checkable' class implements the type-checking algorithm. If
-- type-checking fails, the result is a divergence marker.

class Checkable a where
    check :: a -> TypeCheck

instance Checkable (Parser.Expr Pos) where

    -- An annotation asserts that an expression has an expected type, then
    -- propagates the type specified in the annotation.
    check (Parser.EAnnotation a) = do
        t1 <- resolve (a ^. annotationType)
        t2 <- check (a ^. annotationExpr)
        unify (position a) t1 t2

    check (Parser.EApp a) = undefined
    check (Parser.EHole i) = undefined
    check (Parser.EIf i) = undefined
    check (Parser.EFn ps) = undefined
    check (Parser.ELet l) = undefined
    check (Parser.ELit l) = undefined
    check (Parser.EMatch m) = undefined
    check (Parser.EVar i) = undefined
    check (Parser.EOpen o e) = undefined
