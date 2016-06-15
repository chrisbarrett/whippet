{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Frontend.PPrint where

import           Control.Lens
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Monoid                   ((<>))
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Language.Whippet.Frontend.AST

class PPrint a where
    pprint :: a -> Text

pprint' :: PPrint a => Getter a Text
pprint' = to pprint

instance PPrint Ident where
    pprint = view identLabel

instance PPrint TypeParameter where
    pprint = view (typeParameterIdent.identLabel)

instance PPrint QualId where
    pprint (QualId xs) =
        let ts = map pprint (NonEmpty.toList xs)
        in Text.intercalate "." ts

instance PPrint Field where
     pprint f =
         (f ^. fieldIdent.pprint') <> ": " <> (f ^. fieldType.pprint')

instance PPrint Type where
    pprint (TyNominal i) = pprint i

    pprint (TyVar i) = pprint i

    pprint (TyApp x y) =
        Text.unwords [pprint x, pprint y]

    pprint (TyStructural fs) =
        "{" <> Text.intercalate ", " (map pprint fs) <> "}"

    pprint (TyArrow a b) =
        "(" <> pprint a <> " -> " <> pprint b <> ")"

    pprint (TyForall ps t) =
        "(forall " <> pfmt ps <> ". " <> pprint t <> ")"
        where
          pfmt = Text.unwords . fmap pprint . NonEmpty.toList
