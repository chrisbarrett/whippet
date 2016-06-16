{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Frontend.Parser.FunctionSpec where

import           Control.Lens
import           Control.Lens.Extras
import           Control.Monad                               (when)
import           Data.Text                                   (Text)
import           Language.Whippet.Frontend.AST
import qualified Language.Whippet.Frontend.Parser            as Parser
import           Language.Whippet.Frontend.Parser.ParseUtils
import           Language.Whippet.Frontend.PPrint
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "parsing a function signature" $ do

        let whenParsesToSig result assertions = do
                it "parses to a function signature" $
                    result `shouldSatisfy` is (_Right._AstDecl._DecFunSig)
                when (is _Right result) assertions

            fnType' :: ParsedAst -> Text
            fnType' ast =
                ast ^. _Right._AstDecl._DecFunSig.functionSigType.pprint'

            fnName :: ParsedAst -> Text
            fnName ast =
                ast ^. _Right._AstDecl._DecFunSig.functionSigIdent.pprint'

        describe "unary type signature" $ do
            result <- parseFile "UnitFunSig.whippet"
            whenParsesToSig result $ do
                it "has the expected identifier" $
                    fnName result `shouldBe` "unit"
                it "has the expected type parameters" $
                    fnType' result `shouldBe` "Unit"

        describe "binary type signature" $ do
            result <- parseFile "IdentityFunSig.whippet"
            whenParsesToSig result $ do
                it "has the expected identifier" $
                    fnName result `shouldBe` "identity"
                it "has the expected type parameters" $
                    fnType' result `shouldBe` "(a -> a)"

        describe "ternary type signature" $ do
            result <- parseFile "ConstFunSig.whippet"
            whenParsesToSig result $ do
                it "has the expected identifier" $
                    fnName result `shouldBe` "const"
                it "has the expected type parameters" $
                    fnType' result `shouldBe` "(a -> (b -> a))"

        describe "type signature with paranthesised identifier" $ do
            result <- parseFile "FunctionTyParens.whippet"
            whenParsesToSig result $ do
                it "has the expected identifier" $
                    fnName result `shouldBe` "const"
                it "has the expected type parameters" $
                    fnType' result `shouldBe` "(a -> (b -> a))"

        describe "type signature with type constructor parameter" $ do
            result <- parseFile "FunctionTyCtor.whippet"
            whenParsesToSig result $ do
                it "has the expected identifier" $
                    fnName result `shouldBe` "getOpt"
                it "has the expected type parameters" $
                    fnType' result `shouldBe` "(a -> (Option a -> a))"

        describe "type signature with function type parameter" $ do
            result <- parseFile "ListMapFun.whippet"
            whenParsesToSig result $ do
                it "has the expected identifier" $
                    fnName result `shouldBe` "map"
                it "has the expected type parameters" $
                    fnType' result `shouldBe` "((a -> b) -> (List a -> List b))"

        describe "type signature with structural type as input" $ do
            result <- parseFile "StructuralTypeParameterInput.whippet"
            whenParsesToSig result $ do
                it "has the expected identifier" $
                    fnName result `shouldBe` "first"
                it "has the expected type parameters" $
                    fnType' result `shouldBe` "({fst: A, snd: B} -> A)"

        describe "type signature with structural type as output" $ do
            result <- parseFile "StructuralTypeParameterOutput.whippet"
            whenParsesToSig result $ do
                it "has the expected identifier" $
                    fnName result `shouldBe` "box"
                it "has the expected type parameters" $
                    fnType' result `shouldBe` "(A -> {unpack: A})"

        describe "type signature with existential quantifier" $ do
            result <- parseFile "TypeSignatureWithQuantifier.whippet"
            whenParsesToSig result $ do
                it "has the expected type parameters" $
                    fnType' result `shouldBe` "(forall a. ((forall s. ST s a) -> a))"

        describe "type signature with constraint" $ do
            result <- parseFile "TypeSignatureWithConstraint.whippet"
            whenParsesToSig result $ do
                it "has the expected type signature" $
                    fnType' result `shouldBe` "((Eq a) => (a -> (a -> Bool)))"

        describe "type signature with multiple constraints" $ do
            result <- parseFile "TypeSignatureWithMultipleConstraints.whippet"
            whenParsesToSig result $ do
                it "has the expected type signature" $
                    fnType' result `shouldBe` "((Eq a, Show a) => a)"


    describe "function definitions" $ do

        let whenParsesToFn result assertions = do
                it "parses to a function signature" $
                    result `shouldSatisfy` is (_Right._AstDecl._DecFun)
                when (is _Right result) assertions

            fnParams :: ParsedAst -> [FnParam]
            fnParams ast =
                ast ^. _Right._AstDecl._DecFun.functionParams

            param :: Text -> FnParam
            param n = FnParam (ident n) Nothing

            param' :: Text -> Type -> FnParam
            param' n = FnParam (ident n) . Just

            fnType :: ParsedAst -> [Type]
            fnType ast =
                ast ^.. _Right._AstDecl._DecFun.functionType._Just

            fnType' :: ParsedAst -> Text
            fnType' ast =
                ast ^. _Right._AstDecl._DecFun.functionType._Just.pprint'

            fnName :: ParsedAst -> Text
            fnName ast =
                ast ^. _Right._AstDecl._DecFun.functionIdent.pprint'

            fnBody :: ParsedAst -> [Expr]
            fnBody ast =
                ast ^.. _Right._AstDecl._DecFun.functionBody

        describe "simple function" $ do
            result <- parseFile "IdentityFun.whippet"
            whenParsesToFn result $ do
                it "has the expected identifier" $
                    fnName result `shouldBe` "identity"
                it "has the expected parameters" $
                    fnParams result `shouldBe` [param "x"]
                it "has the expected body" $
                    fnBody result `shouldBe` [var "x"]

        describe "simple function with type parameters" $ do
            result <- parseFile "IdentityFunWithType.whippet"
            whenParsesToFn result $ do
                it "has the expected identifier" $
                    fnName result `shouldBe` "identity"
                it "has the expected parameters" $
                    fnParams result `shouldBe` [param' "x" (tyVar "a")]
                it "has the expected body" $
                    fnBody result `shouldBe` [var "x"]

        describe "simple function with result type annotation" $ do
            result <- parseFile "IdentityFunWithResultType.whippet"
            whenParsesToFn result $ do
                it "has the expected identifier" $
                    fnName result `shouldBe` "identity"
                it "has the expected parameters" $
                    fnParams result `shouldBe` [param "x"]
                it "has the expected result type" $
                    fnType result `shouldBe` [tyVar "a"]
                it "has the expected body" $
                    fnBody result `shouldBe` [var "x"]

        describe "functions with pattern matching" $ do

            describe "example 1" $ do
                result <- parseFile "FunWithPatternMatching1.whippet"
                it "should parse" $
                    result `shouldSatisfy` is _Right

            describe "example 2" $ do
                result <- parseFile "FunWithPatternMatching2.whippet"
                it "should parse" $
                    result `shouldSatisfy` is _Right
