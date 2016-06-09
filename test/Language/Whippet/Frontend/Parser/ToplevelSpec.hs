{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Frontend.Parser.ToplevelSpec where

import           Control.Lens
import           Control.Lens.Extras
import           Control.Monad                               (when)
import qualified Data.ByteString.Internal                    as BS
import qualified Data.List.NonEmpty                          as NonEmpty
import           Data.Text                                   (Text)
import           Language.Whippet.Frontend.AST
import qualified Language.Whippet.Frontend.Parser            as Parser
import           Language.Whippet.Frontend.Parser.ParseUtils
import           Language.Whippet.Frontend.PPrint
import           Test.Hspec
import qualified Text.Trifecta                               as Trifecta

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "parsing modules" $ do
        let body :: ParsedAst -> [AST]
            body ast =
                ast ^. _Right._AstModule.moduleBody

            moduleIdentifier :: ParsedAst -> Text
            moduleIdentifier ast =
                ast ^. _Right._AstModule.moduleId.pprint'

            whenParsesToModule result assertions = do
                it "parses to a module" $
                  result `shouldSatisfy` is (_Right._AstModule)
                when (is _Right result) assertions

        describe "empty module" $ do
            result <- parseFile "EmptyModule.whippet"
            whenParsesToModule result $ do
                it "has the expected identifier" $
                    moduleIdentifier result `shouldBe` "ExampleModule"
                it "has an empty body" $
                    body result `shouldSatisfy` is _Empty


    describe "parsing signatures" $ do

        let identifiers :: ParsedAst -> [Text]
            identifiers ast =
                ast ^.. _Right
                      ._AstSignature
                      .signatureBody
                      .traverse
                      .to getIdent
                      ._Just
              where
                getIdent :: Decl -> Maybe Text
                getIdent (DecFun d)        = Just (d ^. functionIdent.pprint')
                getIdent (DecAbsType d)    = Just (d ^. absTypeIdent.pprint')
                getIdent (DecDataType d)   = Just (d ^. dataTypeIdent.pprint')
                getIdent (DecRecordType d) = Just (d ^. recordTypeIdent.pprint')
                getIdent (DecTypeclass d)  = Just (d ^. typeclassIdent.pprint')
                getIdent (DecInstance d)   = Just (d ^. instanceIdent.pprint')

            identifier :: ParsedAst -> Text
            identifier ast =
                ast ^. _Right._AstSignature.signatureId.pprint'

            types :: ParsedAst -> [Text]
            types ast =
                ast ^.. _Right
                      ._AstSignature
                      .signatureBody
                      .traverse
                      ._DecFun
                      .functionType
                      ._Just
                      .pprint'

            decls :: ParsedAst -> [Decl]
            decls ast =
                ast ^. _Right._AstSignature.signatureBody

            declsCount :: ParsedAst -> Int
            declsCount = length . decls

            whenParsesToSignature result assertions = do
                it "parses to a signature" $
                  result `shouldSatisfy` is (_Right._AstSignature)
                when (is _Right result) assertions

        describe "empty signature" $ do
            result <- parseFile "EmptySignature.whippet"
            whenParsesToSignature result $ do
                it "has the expected identifier" $
                    identifier result `shouldBe` "ExampleSignature"
                it "has an empty body" $
                    decls result `shouldSatisfy` is _Empty

        describe "signature with function decl" $ do
            result <- parseFile "SignatureWithFn.whippet"
            whenParsesToSignature result $ do
                it "has the expected fn name" $
                    identifiers result `shouldBe` ["foo"]
                it "has one inner declaration" $
                    declsCount result `shouldBe` 1
                it "has the expected types" $
                    types result `shouldBe` ["(A -> B)"]

        describe "signature with multiple function decls" $ do
            result <- parseFile "SignatureWithMultipleFns.whippet"
            whenParsesToSignature result $ do
                it "has two inner declarations" $
                    declsCount result `shouldBe` 2
                it "has the expected fn names" $
                    identifiers result `shouldBe` ["foo", "bar"]
                it "has the expected types" $
                    types result `shouldBe` ["(A -> B)", "(B -> C)"]

        describe "signature with abstract type" $ do
            result <- parseFile "SignatureWithAbsType.whippet"
            whenParsesToSignature result $
                it "has the expected type name" $
                    identifiers result `shouldBe` ["T"]

        describe "realistic signature" $ do
            result <- parseFile "Option.whippet"
            whenParsesToSignature result $ do
                it "has the expected module name" $
                    identifier result `shouldBe` "Option"
                it "has the expected identifiers" $
                    identifiers result `shouldBe` [ "T"
                                                  , "some?"
                                                  , "none?"
                                                  , "get"
                                                  , "map"
                                                  , "filter"
                                                  ]


    describe "open statement" $ do

        let parseAst :: BS.ByteString -> ParsedAst
            parseAst =
                resultToEither . Trifecta.parseByteString (Parser.ast <* Trifecta.eof) mempty

            whenParsesToOpen result assertions = do
                it "parses to an 'open' statement" $
                    result `shouldSatisfy` is (_Right._AstOpen)
                when (is _Right result) assertions

            hidden :: ParsedAst -> [Ident]
            hidden ast =
                ast ^. _Right._AstOpen.openHiding._Just

            rename :: ParsedAst -> [Ident]
            rename ast =
                ast ^.. _Right._AstOpen.openAs._Just

            modId :: ParsedAst -> [Ident]
            modId ast =
                ast ^. _Right._AstOpen._Open._1._QualId.to NonEmpty.toList

        describe "simple open" $ do
            let result = parseAst "open M"
            whenParsesToOpen result $
                it "has the expected module ID" $
                    modId result `shouldBe` [ident "M"]

        describe "open module with path" $ do
            let result = parseAst "open M.N"
            whenParsesToOpen result $
                it "has the expected module ID" $
                    modId result `shouldBe` [ident "M", ident "N"]

        describe "open hiding" $ do
            let result = parseAst "open M hiding (foo, bar)"
            whenParsesToOpen result $ do
                it "has the expected module ID" $
                    modId result `shouldBe` [ident "M"]
                it "has the expected hidden identifiers" $
                    hidden result `shouldBe` [ident "foo", ident "bar"]

        describe "optional comma in 'hiding'" $ do
            let result = parseAst "open M hiding (,foo,bar)"
            whenParsesToOpen result $
                it "has the expected hidden identifiers" $
                    hidden result `shouldBe` [ident "foo", ident "bar"]

        describe "open with renaming" $ do
            let result = parseAst "open M as X"
            whenParsesToOpen result $ do
                it "has the expected module ID" $
                    modId result `shouldBe` [ident "M"]
                it "has the expected rebinding" $
                    rename result `shouldBe` [ident "X"]

        describe "open with renaming and hidden" $ do
            let result = parseAst "open M as X hiding (x,y)"
            whenParsesToOpen result $ do
                it "has the expected module ID" $
                    modId result `shouldBe` [ident "M"]
                it "has the expected rebinding" $
                    rename result `shouldBe` [ident "X"]
                it "has the expected hidden identifiers" $
                    hidden result `shouldBe` [ident "x", ident "y"]
