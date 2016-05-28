{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Frontend.ParserSpec where

import           Control.Lens
import           Control.Lens.Extras
import           Data.Monoid                      ((<>))
import           Data.String                      (fromString)
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Debug.Trace
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

parseFile p name = do
    content <- runIO (loadResource name)
    pure (resultToEither (parse content))
  where
    resultToEither :: Trifecta.Result (AST s) -> ParsedAst s
    resultToEither (Trifecta.Success x) = Right x
    resultToEither (Trifecta.Failure e) = Left ("\n" <> e)

    loadResource name = do
        realPath <- Paths.getDataFileName ("test/resources/" <> name)
        readFile realPath

    parse content =
        let delta = Trifecta.Directed (fromString name) 0 0 0 0
        in Trifecta.parseByteString p delta (fromString content)

astHasIdentifier :: Text -> ParsedAst s -> Bool
astHasIdentifier s =
   (==) s . view (_Right.identifier.text)

spec :: Spec
spec = do

    -- Modules

    describe "parsing modules" $ do
        let body :: ParsedAst s -> [AST s]
            body = view (_Right._AstModule._2)

            itParsesToModule :: ParsedAst s -> Spec
            itParsesToModule result =
                it "parses to a module" $
                  result `shouldSatisfy` is (_Right._AstModule)

        context "empty module" $ do
            result <- parseFile Parser.ast "EmptyModule.whippet"
            itParsesToModule result
            it "has the expected identifier" $
                result `shouldSatisfy` astHasIdentifier "ExampleModule"
            it "has an empty body" $
                body result `shouldSatisfy` is _Empty

    -- Signatures

    describe "parsing signatures" $ do

        let identifiers :: ParsedAst s -> [Text]
            identifiers =
                fmap (view (identifier.text)) . view (_Right._AstSignature._2)

            parameters :: ParsedAst s -> [[Text]]
            parameters =
                (fmap.fmap) (view (_TyNominal._2.text))
                . fmap (view (_DecFun._3))
                . view (_Right._AstSignature._2)

            decls :: ParsedAst s -> [Decl s]
            decls = view (_Right._AstSignature._2)

            declsCount :: ParsedAst s -> Int
            declsCount = length . decls

            itParsesToSignature :: ParsedAst s -> Spec
            itParsesToSignature result =
                it "parses to a signature" $
                  result `shouldSatisfy` is (_Right._AstSignature)

        context "empty signature" $ do
            result <- parseFile Parser.ast "EmptySignature.whippet"
            itParsesToSignature result
            it "has the expected identifier" $
                result `shouldSatisfy` astHasIdentifier "ExampleSignature"
            it "has an empty body" $
                decls result `shouldSatisfy` is _Empty

        context "signature with function decl" $ do
            result <- parseFile Parser.ast "SignatureWithFn.whippet"
            itParsesToSignature result
            it "has the expected fn name" $
                identifiers result `shouldBe` ["foo"]
            it "has one inner declaration" $
                declsCount result `shouldBe` 1
            it "has the expected parameters" $
                parameters result `shouldBe` [["A", "B"]]

        context "signature with multiple function decls" $ do
            result <- parseFile Parser.ast "SignatureWithMultipleFns.whippet"
            itParsesToSignature result
            it "has two inner declarations" $
                declsCount result `shouldBe` 2
            it "has the expected fn names" $
                identifiers result `shouldBe` ["foo", "bar"]
            it "has the expected parameters" $
                parameters result `shouldBe` [ ["A", "B"]
                                             , ["B", "C"]
                                             ]

        context "signature with abstract type" $ do
            result <- parseFile Parser.ast "SignatureWithAbsType.whippet"
            itParsesToSignature result
            it "has the expected type name" $
                identifiers result `shouldBe` ["T"]

        -- context "realistic signature" $ do
        --     result <- parseFile Parser.ast "Option.whippet"
        --     itParsesToSignature result


    -- Type declarations

    describe "parsing a record declaration" $ do
        let fieldsFromAst :: ParsedAst s -> [Field s]
            fieldsFromAst =
                view (_Right._AstDecl._DecRecordType._4)

            fieldLabels =
                fmap (view (identifier.text)) . fieldsFromAst

            fieldTypeNames =
                fmap (view (fieldType.to typeIdentifiers._Just.each.text)) . fieldsFromAst

            itParsesToRecordDecl :: ParsedAst s -> Spec
            itParsesToRecordDecl result =
                it "parses to a record decl" $
                    result `shouldSatisfy` is (_Right._AstDecl._DecRecordType)

        context "record type" $ do
            result <- parseFile Parser.ast "IntPair.whippet"
            itParsesToRecordDecl result
            it "has the expected identifier" $
                result `shouldSatisfy` astHasIdentifier "IntPair"
            it "has the expected fields" $
                fieldLabels result `shouldBe` ["fst", "snd"]
            it "has the expected field types" $
                fieldTypeNames result `shouldBe` ["Int", "Int"]

        context "record type with type parameters" $ do
            result <- parseFile Parser.ast "Pair.whippet"
            itParsesToRecordDecl result
            it "has the expected fields" $
                fieldLabels result `shouldBe` ["fst", "snd"]
            it "has the expected field types" $
                fieldTypeNames result `shouldBe` ["a", "b"]

        context "record type with comma before first field" $ do
            result <- parseFile Parser.ast "RecordOptionalLeadingComma.whippet"
            itParsesToRecordDecl result
            it "has the expected fields" $
                fieldLabels result `shouldBe` ["fst", "snd"]

    describe "parsing a type declaration" $ do
        let ctorsFromAst :: ParsedAst s -> [Ctor s]
            ctorsFromAst =
                view (_Right._AstDecl._DecDataType._4)

            typeParameters :: ParsedAst s -> [Text]
            typeParameters =
                fmap (view (identifier.text))
                . view (_Right._AstDecl._DecDataType._3)

            ctorLabels :: ParsedAst s -> [Text]
            ctorLabels =
                fmap (view (identifier.text)) . ctorsFromAst

            ctorParamTypes :: ParsedAst s -> [Text]
            ctorParamTypes =
                fmap (view (to typeIdentifiers._Just.each.text))
                . concatMap (view ctorParams)
                . ctorsFromAst

            itParsesToTypeDecl :: ParsedAst s -> Spec
            itParsesToTypeDecl result =
                it "parses to a type decl" $
                    result `shouldSatisfy` is (_Right._AstDecl._DecDataType)

            itParsesToAbsTypeDecl :: ParsedAst s -> Spec
            itParsesToAbsTypeDecl result =
                it "parses to a type decl" $
                    result `shouldSatisfy` is (_Right._AstDecl._DecAbsType)


        context "abstract type" $ do
            result <- parseFile Parser.ast "Void.whippet"
            itParsesToAbsTypeDecl result
            it "has the expected identifier" $
                result `shouldSatisfy` astHasIdentifier "Void"

        context "nullary constructor" $ do
            result <- parseFile Parser.ast "Unit.whippet"
            itParsesToTypeDecl result
            it "has the expected constructor" $
                ctorLabels result `shouldBe` ["Unit"]
            it "has no parameters" $
                ctorParamTypes result `shouldBe` []

        context "multiple nullary constructors" $ do
            result <- parseFile Parser.ast "Bool.whippet"
            itParsesToTypeDecl result
            it "has the expected constructors" $
                ctorLabels result `shouldBe` ["True", "False"]
            it "has no parameters" $
                ctorParamTypes result `shouldBe` []

        context "first constructor has a leading pipe" $ do
            result <- parseFile Parser.ast "CtorOptionalPipe.whippet"
            itParsesToTypeDecl result
            it "has the expected constructors" $
                ctorLabels result `shouldBe` ["True", "False"]

        context "single type parameter" $ do
            result <- parseFile Parser.ast "PhantomType.whippet"
            itParsesToTypeDecl result
            it "has the expected type parameter" $
                typeParameters result `shouldBe` ["a"]

        context "multiple type parameters" $ do
            result <- parseFile Parser.ast "CoerceType.whippet"
            itParsesToTypeDecl result
            it "has the expected type parameters" $
                typeParameters result `shouldBe` ["source", "dest"]

        context "constructor reference to type parameters" $ do
            result <- parseFile Parser.ast "Either.whippet"
            itParsesToTypeDecl result
            it "has the expected ctor parameter types" $
                ctorParamTypes result `shouldBe` ["e", "a"]

    describe "parsing a function signature" $ do
        let ident :: ParsedAst s -> Text
            ident = view (_Right._AstDecl._DecFun._2.identifier.text)

            tyParameters :: ParsedAst s -> [Text]
            tyParameters =
                fmap tyToText . view (_Right._AstDecl._DecFun._3)

            itParsesToFnSig :: ParsedAst s -> Spec
            itParsesToFnSig result =
                it "parses to a function signature" $
                    result `shouldSatisfy` is (_Right._AstDecl._DecFun)

        context "simple function decl" $ do
            result <- parseFile Parser.ast "IdentityFunSig.whippet"
            itParsesToFnSig result
            it "has the expected identifier" $
                ident result `shouldBe` "identity"
            it "has the expected type parameters" $
                tyParameters result `shouldBe` ["a", "a"]

        context "function decl with type constructor parameter" $ do
            result <- parseFile Parser.ast "FunctionTyCtor.whippet"
            itParsesToFnSig result
            it "has the expected identifier" $
                ident result `shouldBe` "getOpt"
            it "has the expected type parameters" $
                tyParameters result `shouldBe` ["a", "Option a", "a"]

        context "function decl with function type parameter" $ do
            result <- parseFile Parser.ast "ListMapFun.whippet"
            itParsesToFnSig result
            it "has the expected identifier" $
                ident result `shouldBe` "map"
            it "has the expected type parameters" $
                tyParameters result `shouldBe` ["(a -> b)", "List a", "List b"]

        context "function decl with paranthesised identifier" $ do
            result <- parseFile Parser.ast "FunctionTyParens.whippet"
            itParsesToFnSig result
            it "has the expected identifier" $
                ident result `shouldBe` "const"
            it "has the expected type parameters" $
                tyParameters result `shouldBe` ["a", "b", "a"]
