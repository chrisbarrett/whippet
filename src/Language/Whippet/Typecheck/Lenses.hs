{-# LANGUAGE TemplateHaskell #-}
module Language.Whippet.Typecheck.Lenses where

import           Control.Lens.TH
import           Language.Whippet.Typecheck.Errors
import           Language.Whippet.Typecheck.Types

makeLenses ''Type
makePrisms ''Type

makeLenses ''QualId
makePrisms ''QualId

makeLenses ''Ident
makePrisms ''Ident

makeLenses ''Err
makePrisms ''Err

makeLenses ''Error
makePrisms ''Error

makeLenses ''UnificationError
makePrisms ''UnificationError
