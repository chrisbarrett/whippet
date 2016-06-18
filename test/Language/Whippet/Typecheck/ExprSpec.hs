{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Typecheck.ExprSpec where

import           Control.Lens
import           Control.Lens.Extras
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Data.Validation
import           Language.Whippet.Parser.ParseUtils
import qualified Language.Whippet.Typecheck         as Typecheck
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "expressions" $ do

        describe "annotated literals" $ do

            describe "invalid annotation" $ do
                let result = Typecheck.check (int 1 `eann` nominalType "String")
                it "is rejected" $
                    result `shouldSatisfy` is _Failure
