{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Parser.TypeclassSpec where

import           Control.Lens
import           Control.Lens.Extras
import           Control.Monad                      (when)
import           Data.Text                          (Text)
import           Debug.Trace
import           Language.Whippet.Parser            hiding (parseFile)
import           Language.Whippet.Parser.Lenses
import           Language.Whippet.Parser.ParseUtils
import           Language.Whippet.PPrint
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "parsing typeclass declarations" $ do
        let body :: ParsedAst -> [FnOrSig ()]
            body ast =
                ast ^. _Right._AstDecl._DecTypeclass.typeclassDecls

            name :: ParsedAst -> Text
            name ast =
                ast ^. _Right._AstDecl._DecTypeclass.typeclassIdent.pprint'

            declarations :: ParsedAst -> [Text]
            declarations ast =
                ast ^.. _Right
                      ._AstDecl
                      ._DecTypeclass
                      .typeclassDecls
                      .traverse
                      .to (getIdent.toDecl)
                      ._Just

            toDecl :: FnOrSig () -> Decl ()
            toDecl (Fn f)  = DecFun f
            toDecl (Sig f) = DecFunSig f

            whenParsesToTypeclass result assertions = do
                it "parses to a typeclass" $
                  result `shouldSatisfy` is (_Right._AstDecl._DecTypeclass)
                when (is _Right result) assertions

        describe "empty typeclass" $ do
            result <- parseFile "EmptyTypeclass.whippet"
            whenParsesToTypeclass result $ do
                it "has the expected identifier" $
                    name result `shouldBe` "EmptyTypeclass"
                it "has an empty body" $
                    body result `shouldSatisfy` is _Empty

        describe "typeclass with functions" $ do
            result <- parseFile "TypeclassWithFunctions.whippet"
            whenParsesToTypeclass result $
                it "has the expected declarations" $
                    declarations result `shouldBe` ["foo", "bar"]

    describe "parsing typeclass instances" $ do
        let body :: ParsedAst -> [Function ()]
            body ast =
                ast ^. _Right._AstDecl._DecInstance.instanceDecls

            className :: ParsedAst -> [Text]
            className ast =
                ast ^.. _Right._AstDecl._DecInstance.instanceIdent.pprint'

            targetName :: ParsedAst -> Text
            targetName ast =
                ast ^. _Right._AstDecl._DecInstance.instanceTarget.pprint'

            declarations :: ParsedAst -> [Text]
            declarations ast =
                ast ^.._Right
                      ._AstDecl
                      ._DecInstance
                      .instanceDecls
                      .traverse
                      .functionIdent
                      .pprint'

            whenParsesToTypeclass result assertions = do
                it "parses to a typeclass instance" $
                  result `shouldSatisfy` is (_Right._AstDecl._DecInstance)
                when (is _Right result) assertions

        describe "empty typeclass instance" $ do
            result <- parseFile "EmptyInstance.whippet"
            whenParsesToTypeclass result $ do
                it "has the expected typeclass name" $
                    className result `shouldBe` ["EmptyTypeclass"]
                it "has an empty body" $
                    body result `shouldSatisfy` is _Empty
                it "has the expected target type name" $
                    targetName result `shouldBe` "Bool"

        -- -- FIXME: Implement function parsing.
        -- describe "typeclass instance with functions" $ do
        --     result <- parseFile "InstanceWithFunctions.whippet"
        --     whenParsesToTypeclass result $ do
        --         it "has the expected declarations" $
        --             declarations result `shouldBe` ["foo", "bar"]
        --         it "has the expected typeclass name" $
        --             className result `shouldBe` ["ExampleTypeclass"]
        --         it "has the expected target type name" $
        --             targetName result `shouldBe` "Int"
