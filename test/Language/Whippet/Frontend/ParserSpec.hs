{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Frontend.ParserSpec where

import           Control.Lens
import           Control.Lens.Extras
import           Data.Monoid                      ((<>))
import           Data.String                      (fromString)
import           Data.Text                        (Text)
import           Language.Whippet.Frontend.AST
import qualified Language.Whippet.Frontend.Parser as Parser
import qualified Paths_whippet                    as Paths
import           System.FilePath.Posix            as FilePath
import           Test.Hspec
import           Text.PrettyPrint.ANSI.Leijen     (Doc)
import qualified Text.Trifecta                    as Trifecta
import qualified Text.Trifecta.Delta              as Trifecta
import qualified Text.Trifecta.Result             as Trifecta

type ParsedAst s = Either Doc (AST s)

main :: IO ()
main = hspec spec

parseFile s = runIO $ do
    file <- Paths.getDataFileName ("test/resources/" <> s)
    content <- readFile file

    -- KLUDGE: Use the 'parseByteString' function instead of 'parseFile' so the
    -- path in the error reports are more legible.
    let filename = FilePath.takeFileName file
        delta = Trifecta.Directed (fromString filename) 0 0 0 0
        res = Trifecta.parseByteString Parser.ast delta (fromString content)

    case res of
      Trifecta.Success x -> pure (Right x)
      Trifecta.Failure e -> pure (Left ("\n" <> e))

astHasIdentifier :: Text -> ParsedAst s -> Bool
astHasIdentifier s =
   (==) s . view (_Right.identifier.text)

spec :: Spec
spec = do

    -- Modules

    describe "parsing modules" $ do
        let body :: ParsedAst s -> [AST s]
            body = view (_Right._AstModule._3)

        context "empty module" $ do
            result <- parseFile "EmptyModule.whippet"
            it "returns a module" $
                result `shouldSatisfy` is (_Right._AstModule)
            it "has the expected identifier" $
                result `shouldSatisfy` astHasIdentifier "ExampleModule"
            it "has an empty body" $
                body result `shouldSatisfy` is _Empty

    -- Signatures

    describe "parsing signatures" $ do

        let identifiers :: ParsedAst s -> [Text]
            identifiers =
                fmap (view (identifier.text)) . view (_Right._AstSignature._3)

            parameters :: ParsedAst s -> [Text]
            parameters =
                fmap (view (_TyNominal._2.text))
                . concatMap (view (_FnDecl._3))
                . view (_Right._AstSignature._3)

            decls :: ParsedAst s -> [Decl s]
            decls = view (_Right._AstSignature._3)

            declsCount :: ParsedAst s -> Int
            declsCount = length . decls

        context "empty signature" $ do
            result <- parseFile "EmptySignature.whippet"
            it "returns a signature" $
                result `shouldSatisfy` is (_Right._AstSignature)
            it "has the expected identifier" $
                result `shouldSatisfy` astHasIdentifier "ExampleSignature"
            it "has an empty body" $
                decls result `shouldSatisfy` is _Empty

        context "signature with function decl" $ do
            result <- parseFile "SignatureWithFn.whippet"
            it "has the expected fn name" $
                identifiers result `shouldBe` ["foo"]
            it "has one inner declaration" $
                declsCount result `shouldBe` 1
            it "has the expected parameters" $
                parameters result `shouldBe` ["A", "B"]

    -- Type declarations

    describe "parsing a record declaration" $ do
        let fieldsFromAst :: ParsedAst s -> [Field s]
            fieldsFromAst =
                view (_Right._AstRecordType._4)

            fieldLabels =
                fmap (view (identifier.text)) . fieldsFromAst

            fieldTypeNames =
                fmap (view (fieldType.typeIdentifier._Just.text)) . fieldsFromAst

        context "record type" $ do
            result <- parseFile "IntPair.whippet"
            it "returns a type declaration" $
                result `shouldSatisfy` is (_Right._AstRecordType)
            it "has the expected identifier" $
                result `shouldSatisfy` astHasIdentifier "IntPair"
            it "has the expected fields" $
                fieldLabels result `shouldBe` ["fst", "snd"]
            it "has the expected field types" $
                fieldTypeNames result `shouldBe` ["Int", "Int"]

        context "record type with type parameters" $ do
            result <- parseFile "Pair.whippet"
            it "has the expected fields" $
                fieldLabels result `shouldBe` ["fst", "snd"]
            it "has the expected field types" $
                fieldTypeNames result `shouldBe` ["a", "b"]

        context "record type with comma before first field" $ do
            result <- parseFile "RecordOptionalLeadingComma.whippet"
            it "returns a type declaration" $
                result `shouldSatisfy` is (_Right._AstRecordType)
            it "has the expected fields" $
                fieldLabels result `shouldBe` ["fst", "snd"]



    describe "parsing a type declaration" $ do
        let ctorsFromAst :: ParsedAst s -> [Ctor s]
            ctorsFromAst =
                view (_Right._AstDataType._4)

            typeParameters :: ParsedAst s -> [Text]
            typeParameters =
                fmap (view (identifier.text)) . view (_Right._AstDataType._3)

            ctorLabels :: ParsedAst s -> [Text]
            ctorLabels =
                fmap (view (identifier.text)) . ctorsFromAst


            absTypeParams :: ParsedAst s -> [Text]
            absTypeParams =
                fmap (view (identifier.text)) . view (_Right._AstAbstractType._3)

            ctorParamTypes :: ParsedAst s -> [Text]
            ctorParamTypes =
                fmap (view (typeIdentifier._Just.text))
                . concatMap (view ctorParams)
                . ctorsFromAst


        context "abstract type" $ do
            result <- parseFile "Void.whippet"
            it "returns a type declaration" $
                result `shouldSatisfy` is (_Right._AstAbstractType)
            it "has the expected identifier" $
                result `shouldSatisfy` astHasIdentifier "Void"

        context "nullary constructor" $ do
            result <- parseFile "Unit.whippet"
            it "has the expected constructor" $
                ctorLabels result `shouldBe` ["Unit"]
            it "has no parameters" $
                ctorParamTypes result `shouldBe` []

        context "multiple nullary constructors" $ do
            result <- parseFile "Bool.whippet"
            it "has the expected constructors" $
                ctorLabels result `shouldBe` ["True", "False"]
            it "has no parameters" $
                ctorParamTypes result `shouldBe` []

        context "first constructor has a leading pipe" $ do
            result <- parseFile "CtorOptionalPipe.whippet"
            it "has the expected constructors" $
                ctorLabels result `shouldBe` ["True", "False"]

        context "single type parameter" $ do
            result <- parseFile "PhantomType.whippet"
            it "has the expected type parameter" $
                typeParameters result `shouldBe` ["a"]

        context "multiple type parameters" $ do
            result <- parseFile "CoerceType.whippet"
            it "has the expected type parameters" $
                absTypeParams result `shouldBe` ["source", "dest"]

        context "constructor reference to type parameters" $ do
            result <- parseFile "Either.whippet"
            it "has the expected ctor parameter types" $
                ctorParamTypes result `shouldBe` ["e", "a"]
