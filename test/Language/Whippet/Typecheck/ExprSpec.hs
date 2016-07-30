{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Typecheck.ExprSpec where

import           Control.Lens
import           Control.Lens.Extras
import           Control.Monad.Identity
import           Data.Sequence                      (Seq)
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import qualified Language.Whippet.Parser            as Parser
import qualified Language.Whippet.Parser.HasPos     as HasPos
import           Language.Whippet.Parser.ParseUtils
import qualified Language.Whippet.Typecheck         as Typecheck
import           Test.Hspec

main :: IO ()
main = hspec spec

runCheck :: Parser.Expr () -> Either (Seq Typecheck.Err) Typecheck.Type
runCheck =
    Typecheck.typecheck . fmap (const HasPos.emptyPos)

spec :: Spec
spec = do
    describe "expressions" $ do
        let fullyQualifiedStringType     = "Language.Whippet.Prim.String"
            fullyQualifiedIntType        = "Language.Whippet.Prim.Int"
            fullyQualifiedCharType       = "Language.Whippet.Prim.Char"
            fullyQualifiedScientificType = "Language.Whippet.Prim.Scientific"
            fullyQualifiedSeqType        = "Language.Whippet.Prim.Seq"

        describe "literals" $ do

            describe "string literal" $ do
                let result = runCheck (str "foo")
                it "is accepted" $
                    result `shouldSatisfy` is _Right
                it "has the expected type" $
                    result ^.. _Right `shouldBe` [Typecheck.TyNominal fullyQualifiedStringType]

            describe "int literal" $ do
                let result = runCheck (int 1)
                it "is accepted" $
                    result `shouldSatisfy` is _Right
                it "has the expected type" $
                    result ^.. _Right `shouldBe` [Typecheck.TyNominal fullyQualifiedIntType]

            describe "char literal" $ do
                let result = runCheck (char 'a')
                it "is accepted" $
                    result `shouldSatisfy` is _Right
                it "has the expected type" $
                    result ^.. _Right `shouldBe` [Typecheck.TyNominal fullyQualifiedCharType]

            describe "scientific literal" $ do
                let result = runCheck (scientific 1)
                it "is accepted" $
                    result `shouldSatisfy` is _Right
                it "has the expected type" $
                    result ^.. _Right `shouldBe` [Typecheck.TyNominal fullyQualifiedScientificType]

            describe "list literal" $ do
                let tySeqOf = Typecheck.TyApp (Typecheck.TyNominal fullyQualifiedSeqType)
                    tyInt = Typecheck.TyNominal fullyQualifiedIntType

                describe "homogenous children" $ do
                    let result = runCheck (list [int 1, int 2])
                    it "is accepted" $
                        result `shouldSatisfy` is _Right
                    it "has expected type" $
                        result ^.. _Right `shouldBe` [tySeqOf tyInt]

                describe "heterogeneous children" $ do
                    let result = runCheck (list [int 1, str "2"])
                    it "is rejected" $
                        result `shouldSatisfy` is _Left


        describe "type annotations" $ do
            let ann e t = Parser.EAnnotation (Parser.Annotation Nothing e t)
                tyString = nominalType fullyQualifiedStringType

            describe "invalid annotation" $ do
                let result = runCheck (int 1 `ann` tyString)
                it "is rejected" $
                    result `shouldSatisfy` is _Left

            describe "valid annotation" $ do
                let result = runCheck (str "foo" `ann` tyString)
                it "is accepted" $
                    result `shouldSatisfy` is _Right
