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

instance PPrint (Ident a) where
    pprint = view identLabel

instance PPrint (TypeParameter a) where
    pprint = view (typeParameterIdent.identLabel)

instance PPrint (QualId a) where
    pprint (QualId xs) =
        Text.intercalate "." (xs ^.. traverse.pprint')

instance PPrint (Field a) where
     pprint f = name <> ": " <> ty
       where
         name = f ^. fieldIdent.pprint'
         ty = f ^. fieldType.pprint'

instance PPrint (Type a) where
    pprint (TyNominal _ i) = pprint i

    pprint (TyVar _ i) = pprint i

    pprint (TyApp _ x y) =
        Text.unwords [pprint x, pprint y]

    pprint (TyStructural _ fs) =
        "{" <> fields <> "}"
      where
        fields = Text.intercalate ", " (fs ^.. traverse.pprint')

    pprint (TyArrow _ a b) =
        "(" <> pprint a <> " -> " <> pprint b <> ")"

    pprint (TyForall _ ps t) =
        "(forall " <> binders <> ". " <> pprint t <> ")"
        where
          binders = Text.unwords (ps ^.. traverse.pprint')

    pprint (TyConstraint _ t u) =
        "((" <> constraints <> ") => " <> pprint u <> ")"
      where
        constraints = Text.intercalate ", " (t ^.. traverse.pprint')

instance PPrint (Constraint a) where
    pprint c = Text.unwords (ctor : params)
      where
        ctor = c ^. constraintCtor.pprint'
        params = c ^.. constraintParams.traverse.pprint'
