{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Frontend.ParserSpec where

import           Control.Lens
import           Control.Lens.Extras
import           Data.Monoid                          ((<>))
import           Data.String                          (fromString)
import           Data.Text                            (Text)
import           Language.Whippet.Frontend.AST
import           Language.Whippet.Frontend.ASTHelpers
import qualified Language.Whippet.Frontend.Parser     as Parser
import qualified Paths_whippet                        as Paths
import           System.FilePath.Posix                as FilePath
import           Test.Hspec
import qualified Text.Trifecta                        as Trifecta
import qualified Text.Trifecta.Delta                  as Trifecta
import qualified Text.Trifecta.Result                 as Trifecta

main :: IO ()
main = hspec spec

parseFile s = runIO $ do
    file <- Paths.getDataFileName ("test/resources/" <> s)
    content <- readFile file

    -- KLUDGE: Use the 'parseByteString' function instead of 'parseFile' so the
    -- path in the error reports are more legible.
    let filename = FilePath.takeFileName file
        delta = Trifecta.Directed (fromString filename) 0 0 0 0
        res = Trifecta.parseByteString Parser.topLevel delta (fromString content)

    case res of
      Trifecta.Success x -> pure (Right x)
      Trifecta.Failure e -> pure (Left ("\n" <> e))

decls :: AST s -> [Decl s]
decls r =
    case r of
        AstModule _ _ ds    -> ds
        AstSignature _ _ ds -> ds
        _                   -> error "No inner decls"

hasIdentifier :: Text -> Either e (AST s) -> Bool
hasIdentifier s =
   (==) s . view (_Right.identifier.text)

spec :: Spec
spec = do

    -- Modules

    describe "parsing an empty module" $ do
        result <- parseFile "1.whippet"
        it "returns a module" $
            result `shouldSatisfy` is (_Right._AstModule)
        it "has the expected identifier" $
            result `shouldSatisfy` hasIdentifier "ExampleModule"
        it "has an empty body" $
            result `shouldSatisfy` is (_Right._Empty) . fmap decls

    -- Signatures

    describe "parsing an empty signature" $ do
        result <- parseFile "2.whippet"
        it "returns a signature" $
            result `shouldSatisfy` is (_Right._AstSignature)
        it "has the expected identifier" $
            result `shouldSatisfy` hasIdentifier "ExampleSignature"
        it "has an empty body" $
            result `shouldSatisfy` is (_Right._Empty) . fmap decls

    -- Type declarations

    describe "parsing a type declaration" $ do

        let ctorsFromAst :: Either e (AST s) -> [Ctor s]
            ctorsFromAst =
                view (_Right._AstDataType._4)

            typeHasTypeParams :: [Text] -> Either e (AST s) -> Bool
            typeHasTypeParams cs =
                (==) cs
                . fmap (view (identifier.text))
                . view (_Right._AstDataType._3)

            absTypeHasTypeParams :: [Text] -> Either e (AST s) -> Bool
            absTypeHasTypeParams cs =
                (==) cs
                . fmap (view (identifier.text))
                . view (_Right._AstAbstractType._3)

            hasCtorsLabelled :: [Text] -> Either e (AST s) -> Bool
            hasCtorsLabelled cs =
                (==) cs
                . fmap (view (identifier.text))
                . ctorsFromAst

            hasCtorParamsWithTypes :: [Text] -> Either e (AST s) -> Bool
            hasCtorParamsWithTypes ps = do
                (==) ps
                . fmap (view (identifier.text))
                . concatMap (view ctorParams)
                . ctorsFromAst

            fieldsFromAst :: Either e (AST s) -> [Field s]
            fieldsFromAst =
                view (_Right._AstRecordType._4)

            hasFieldsLabelled :: [Text] -> Either e (AST s) -> Bool
            hasFieldsLabelled ps =
                (==) ps
                . fmap (view (identifier.text))
                . fieldsFromAst

            hasFieldTypesNamed :: [Text] -> Either e (AST s) -> Bool
            hasFieldTypesNamed ps =
                (==) ps
                . map (view (fieldType.identifier.text))
                . fieldsFromAst

        context "abstract type" $ do
            result <- parseFile "3.whippet"
            it "returns a type declaration" $
                result `shouldSatisfy` is (_Right._AstAbstractType)
            it "has the expected identifier" $
                result `shouldSatisfy` hasIdentifier "Void"

        context "nullary constructor" $ do
            result <- parseFile "4.whippet"
            it "returns a type declaration" $
                result `shouldSatisfy` is (_Right._AstDataType)
            it "has the expected identifier" $
                result `shouldSatisfy` hasIdentifier "Unit"
            it "has the expected constructor" $
                result `shouldSatisfy` hasCtorsLabelled ["Unit"]
            it "has no parameters" $
                result `shouldSatisfy` hasCtorParamsWithTypes []

        context "multiple nullary constructors" $ do
            result <- parseFile "5.whippet"
            it "returns a type declaration" $
                result `shouldSatisfy` is (_Right._AstDataType)
            it "has the expected identifier" $
                result `shouldSatisfy` hasIdentifier "Bool"
            it "has the expected constructors" $
                result `shouldSatisfy` hasCtorsLabelled ["True", "False"]
            it "has no parameters" $
                result `shouldSatisfy` hasCtorParamsWithTypes []

        context "first constructor has a leading pipe" $ do
            result <- parseFile "11.whippet"
            it "has the expected constructors" $
                result `shouldSatisfy` hasCtorsLabelled ["True", "False"]

        context "single type parameter" $ do
            result <- parseFile "6.whippet"
            it "has the expected type parameter" $
                result `shouldSatisfy` typeHasTypeParams ["a"]

        context "multiple type parameters" $ do
            result <- parseFile "7.whippet"
            it "has the expected type parameters" $
                result `shouldSatisfy` absTypeHasTypeParams ["source", "dest"]

        context "record type" $ do
            result <- parseFile "8.whippet"
            it "returns a type declaration" $
                result `shouldSatisfy` is (_Right._AstRecordType)
            it "has the expected identifier" $
                result `shouldSatisfy` hasIdentifier "IntPair"
            it "has the expected fields" $
                result `shouldSatisfy` hasFieldsLabelled ["fst", "snd"]
            it "has the expected field types" $
                result `shouldSatisfy` hasFieldTypesNamed ["Int", "Int"]

        context "record type with type parameters" $ do
            result <- parseFile "9.whippet"
            it "returns a type declaration" $
                result `shouldSatisfy` is (_Right._AstRecordType)
            it "has the expected identifier" $
                result `shouldSatisfy` hasIdentifier "Pair"
            it "has the expected fields" $
                result `shouldSatisfy` hasFieldsLabelled ["fst", "snd"]
            it "has the expected field types" $
                result `shouldSatisfy` hasFieldTypesNamed ["a", "b"]

        context "constructor reference to type parameters" $ do
            result <- parseFile "10.whippet"
            it "has the expected type parameters" $
                result `shouldSatisfy` typeHasTypeParams ["e", "a"]
            it "has the expected ctor parameter types" $
                result `shouldSatisfy` hasCtorParamsWithTypes ["e", "a"]
