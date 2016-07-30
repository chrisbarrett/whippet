{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Typecheck (
      module Language.Whippet.Typecheck.Errors
    , module Language.Whippet.Typecheck.Lenses
    , module Language.Whippet.Typecheck.Types
    , Checkable (..)
    , TypeCheck
    , typecheck
    ) where

import           Control.Lens
import qualified Data.Foldable                     as Foldable
import           Data.Sequence                     (Seq)
import qualified Data.Sequence                     as Seq
import qualified Data.Text                         as Text
import qualified Language.Whippet.Parser.HasPos    as HasPos
import           Language.Whippet.Parser.Lenses
import qualified Language.Whippet.Parser.Types     as Parser
import           Language.Whippet.Typecheck.CheckM
import           Language.Whippet.Typecheck.Errors
import           Language.Whippet.Typecheck.Lenses
import           Language.Whippet.Typecheck.Types
import qualified Text.Trifecta                     as Trifecta

type TypeCheck = CheckM Type

resolve :: Parser.Type a -> TypeCheck
resolve (Parser.TyNominal _ (Parser.QualId path)) =
    let joined = Text.intercalate "." (path ^.. traverse.identLabel)
    in pure (TyNominal joined)

-- |Return the first of the two types if they unify, otherwise add a type error
-- to the type checking context and return a divergence marker.
unify :: Trifecta.Span -> Type -> Type -> TypeCheck

-- Divergences are subsumed by the expected type in order to continue
-- type-checking.
unify _ t TyDiverge = success t
unify _ TyDiverge t = success t

unify p t1 t2
    | t1 == t2 = success t1
    | otherwise = unificationError p t1 t2
  where
    unificationError s t1 t2 = failure (Err s e)
        where
          e = ErrorUnification (UnificationError t1 t2)

-- * Type-checker

-- |Run the type checker.
typecheck :: Checkable a => a -> Either (Seq Err) Type
typecheck c =
    let (t, state) = runCheckM (check c)
        errs = state ^. checkStateErrs
    in
      if Seq.null errs
        then Right t
        else Left errs

-- |The 'Checkable' class implements the type-checking algorithm. If
-- type-checking fails, the result is a divergence marker.

class Checkable a where
    check :: a -> TypeCheck


instance Checkable (Parser.Expr HasPos.Pos) where
    check e =
        case e of
          -- An annotation asserts that an expression has an expected type, then
          -- propagates the type specified in the annotation.
          Parser.EAnnotation a -> do
              t1 <- resolve (a ^. annotationType)
              t2 <- check (a ^. annotationExpr)
              unify HasPos.emptyPos t1 t2

          Parser.ELit l -> check l

          -- Parser.EApp a -> undefined
          -- Parser.EHole i -> undefined
          -- Parser.EIf i -> undefined
          -- Parser.EFn ps -> undefined
          -- Parser.ELet l -> undefined
          -- Parser.EMatch m -> undefined
          -- Parser.EVar i -> undefined
          -- Parser.EOpen o e -> undefined


instance Checkable (Parser.Lit HasPos.Pos) where

    check Parser.LitInt {} =
        success (TyNominal "Language.Whippet.Prim.Int")

    check Parser.LitChar {} =
        success (TyNominal "Language.Whippet.Prim.Char")

    check Parser.LitScientific {} =
        success (TyNominal "Language.Whippet.Prim.Scientific")

    check Parser.LitRecord {} =
        undefined

    check Parser.LitString {} =
        success (TyNominal "Language.Whippet.Prim.String")

    -- An empty list literal has a polymorphic type.
    check (Parser.LitList []) = do
            var <- freshTyVar
            success (TyApp (TyNominal "Language.Whippet.Prim.Seq") var)

    -- Otherwise a list must have values of a homogeneous type.
    check (Parser.LitList (x : xs)) = do
        t1 <- check x
        Foldable.traverse_ (\x -> check x >>= unify HasPos.emptyPos t1) xs
        success (TyApp (TyNominal "Language.Whippet.Prim.Seq") t1)
