{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Typecheck.ExprSpec where

import           Control.Lens
import           Control.Lens.Extras
import           Control.Monad.Identity
import           Data.Sequence                      (Seq)
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import qualified Language.Whippet.Parser            as Parser
import           Language.Whippet.Parser.ParseUtils
import qualified Language.Whippet.Typecheck         as Typecheck
import           Test.Hspec

main :: IO ()
main = hspec spec

runCheck :: Parser.Expr () -> Either (Seq Typecheck.Err) Typecheck.Type
runCheck =
    Typecheck.toEither . Typecheck.check . fmap (const emptySpan)


spec :: Spec
spec = do
    describe "expressions" $ do

        describe "annotated literals" $ do

            describe "invalid annotation" $ do
                let result = runCheck (int 1 `eann` nominalType "String")
                it "is rejected" $ do
                    pending
                    result `shouldSatisfy` is _Left
