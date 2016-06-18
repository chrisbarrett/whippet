{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Whippet.Typecheck.Types where

import           Data.List.NonEmpty   (NonEmpty)
import           Data.String
import           Data.Text
import qualified Language.Whippet.AST as AST

data Type
    = TyNominal Text
    | TyVar Text
    | TyDiverge
    deriving (Show, Eq, Ord)

newtype Ident = Ident {unIdent :: Text}
    deriving (Eq, Show, IsString)

data QualId = QualId {
      _qualIdModule :: NonEmpty Ident
    , _qualIdName   :: Ident
    }
    deriving (Eq, Show)
