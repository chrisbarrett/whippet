{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Whippet.Frontend.ASTHelpers where

import           Control.Lens
import           Data.Text                     (Text)
import           Language.Whippet.Frontend.AST

class HasPosition a s where
    position :: Lens' (a s) s

label :: Lens' (Ident s) Text
label = identLabel
