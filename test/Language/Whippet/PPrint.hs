{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.PPrint where

import           Control.Lens
import           Data.List.NonEmpty             (NonEmpty)
import qualified Data.List.NonEmpty             as NonEmpty
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Language.Whippet.Parser
import           Language.Whippet.Parser.Lenses

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
        Text.intercalate "." (xs ^.. traverse.pprint')

instance PPrint Field where
     pprint f = name <> ": " <> ty
       where
         name = f ^. fieldIdent.pprint'
         ty = f ^. fieldType.pprint'

instance PPrint Type where
    pprint (TyNominal i) = pprint i

    pprint (TyVar i) = pprint i

    pprint (TyApp x y) =
        Text.unwords [pprint x, pprint y]

    pprint (TyStructural fs) =
        "{" <> fields <> "}"
      where
        fields = Text.intercalate ", " (fs ^.. traverse.pprint')

    pprint (TyArrow a b) =
        "(" <> pprint a <> " -> " <> pprint b <> ")"

    pprint (TyForall ps t) =
        "(forall " <> binders <> ". " <> pprint t <> ")"
        where
          binders = Text.unwords (ps ^.. traverse.pprint')

    pprint (TyConstraint t u) =
        "((" <> constraints <> ") => " <> pprint u <> ")"
      where
        constraints = Text.intercalate ", " (t ^.. traverse.pprint')

instance PPrint Constraint where
    pprint c = Text.unwords (ctor : params)
      where
        ctor = c ^. constraintCtor.pprint'
        params = c ^.. constraintParams.traverse.pprint'
