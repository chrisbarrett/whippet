{-# LANGUAGE OverloadedStrings #-}
module Errors where

import           Control.Lens
import qualified Data.Text                         as Text
import           Language.Whippet.Parser.Lenses
import           Language.Whippet.Typecheck
import           Language.Whippet.Typecheck.Lenses
import           Text.PrettyPrint.ANSI.Leijen      ((<>))
import qualified Text.PrettyPrint.ANSI.Leijen      as PP

pprint :: FilePath -> Err -> PP.Doc
pprint file e =
      renderLocation (e ^. errLoc) <> renderErr (e ^. errError)
  where
    renderLocation p =
        PP.text file
          <> ":"
          <> PP.text (p ^. posLine.unLine.to show)
          <> ":"
          <> PP.text (p ^. posColumn.unCol.to show)
          <> ":"

    renderErr (ErrorUnification e) =
        let expected = ppTy (e ^. unificationErrorExpected)
            actual   = ppTy (e ^. unificationErrorActual)
        in
          "Type error: "
                <> quotes actual
                <> " was different from expected type "
                <> quotes expected <> "."

fromText :: Text.Text -> PP.Doc
fromText = PP.text . Text.unpack

ppTy :: Type -> PP.Doc
ppTy (TyNominal t) = fromText t
ppTy (TyVar t)     = fromText t
ppTy TyDiverge = "⊥"

-- Nested type applications are parenthesised.
ppTy (TyApp t1 t2) =
    case t2 of
      t2@TyApp {} -> ppTy t1 <> " " <> parens (ppTy t2)
      otherwise   -> ppTy t1 <> " " <> ppTy t2

quotes :: PP.Doc -> PP.Doc
quotes d = "`" <> d <> "`"

parens :: PP.Doc -> PP.Doc
parens d = "(" <> d <> ")"
