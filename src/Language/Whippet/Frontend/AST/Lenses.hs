{-# LANGUAGE TemplateHaskell #-}
-- |AST lenses must be implemented as orphans due to the Template Haskell stage
-- restriction prohibiting mutually recursive types.
module Language.Whippet.Frontend.AST.Lenses where

import           Control.Lens                        (Lens')
import           Control.Lens.TH
import           Data.Text                           (Text)
import           Language.Whippet.Frontend.AST.Types

makeLenses ''Ident
makePrisms ''Ident

makeLenses ''Field
makePrisms ''Field

makeLenses ''Type
makePrisms ''Type

makeLenses ''TypeParameter
makePrisms ''TypeParameter

makeLenses ''Decl
makePrisms ''Decl

makeLenses ''Ctor
makePrisms ''Ctor

makeLenses ''AST
makePrisms ''AST

makeLenses ''Pat
makePrisms ''Pat

makeLenses ''Discriminator
makePrisms ''Discriminator

makeLenses ''Expr
makePrisms ''Expr

makeLenses ''Lit
makePrisms ''Lit

makeLenses ''Open
makePrisms ''Open

makeLenses ''QualId
makePrisms ''QualId

makeLenses ''FnParam
makePrisms ''FnParam

makeLenses ''Function
makePrisms ''Function

makeLenses ''AbsType
makePrisms ''AbsType

makeLenses ''DataType
makePrisms ''DataType

makeLenses ''RecordType
makePrisms ''RecordType

makeLenses ''Typeclass
makePrisms ''Typeclass

makeLenses ''Instance
makePrisms ''Instance

makeLenses ''Annotation
makePrisms ''Annotation

makeLenses ''Let
makePrisms ''Let

makeLenses ''If
makePrisms ''If

makeLenses ''Match
makePrisms ''Match

makeLenses ''Signature
makePrisms ''Signature

makeLenses ''Module
makePrisms ''Module

makeLenses ''App
makePrisms ''App

text :: Lens' Ident Text
text = identLabel
