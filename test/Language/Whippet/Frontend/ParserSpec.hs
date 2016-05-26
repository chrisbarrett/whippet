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
import qualified Text.Trifecta                    as Trifecta
import qualified Text.Trifecta.Delta              as Trifecta
import qualified Text.Trifecta.Result             as Trifecta

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
        result <- parseFile "EmptyModule.whippet"
        it "returns a module" $
            result `shouldSatisfy` is (_Right._AstModule)
        it "has the expected identifier" $
            result `shouldSatisfy` hasIdentifier "ExampleModule"
        it "has an empty body" $
            result `shouldSatisfy` is (_Right._Empty) . fmap decls

    -- Signatures

    describe "parsing an empty signature" $ do
        result <- parseFile "EmptySignature.whippet"
        it "returns a signature" $
            result `shouldSatisfy` is (_Right._AstSignature)
        it "has the expected identifier" $
            result `shouldSatisfy` hasIdentifier "ExampleSignature"
        it "has an empty body" $
            result `shouldSatisfy` is (_Right._Empty) . fmap decls

    -- Type declarations

    describe "parsing a record declaration" $ do
        let fieldsFromAst :: Either e (AST s) -> [Field s]
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
                . fmap (view (fieldType.typeIdentifier._Just.text))
                . fieldsFromAst

        context "record type" $ do
            result <- parseFile "IntPair.whippet"
            it "returns a type declaration" $
                result `shouldSatisfy` is (_Right._AstRecordType)
            it "has the expected identifier" $
                result `shouldSatisfy` hasIdentifier "IntPair"
            it "has the expected fields" $
                result `shouldSatisfy` hasFieldsLabelled ["fst", "snd"]
            it "has the expected field types" $
                result `shouldSatisfy` hasFieldTypesNamed ["Int", "Int"]

        context "record type with type parameters" $ do
            result <- parseFile "Pair.whippet"
            it "has the expected fields" $
                result `shouldSatisfy` hasFieldsLabelled ["fst", "snd"]
            it "has the expected field types" $
                result `shouldSatisfy` hasFieldTypesNamed ["a", "b"]

        context "record type with comma before first field" $ do
            result <- parseFile "RecordOptionalLeadingComma.whippet"
            it "returns a type declaration" $
                result `shouldSatisfy` is (_Right._AstRecordType)
            it "has the expected fields" $
                result `shouldSatisfy` hasFieldsLabelled ["fst", "snd"]



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
            hasCtorParamsWithTypes ps =
                (==) ps
                . fmap (view (typeIdentifier._Just.text))
                . concatMap (view ctorParams)
                . ctorsFromAst

        context "abstract type" $ do
            result <- parseFile "Void.whippet"
            it "returns a type declaration" $
                result `shouldSatisfy` is (_Right._AstAbstractType)
            it "has the expected identifier" $
                result `shouldSatisfy` hasIdentifier "Void"

        context "nullary constructor" $ do
            result <- parseFile "Unit.whippet"
            it "has the expected constructor" $
                result `shouldSatisfy` hasCtorsLabelled ["Unit"]
            it "has no parameters" $
                result `shouldSatisfy` hasCtorParamsWithTypes []

        context "multiple nullary constructors" $ do
            result <- parseFile "Bool.whippet"
            it "has the expected constructors" $
                result `shouldSatisfy` hasCtorsLabelled ["True", "False"]
            it "has no parameters" $
                result `shouldSatisfy` hasCtorParamsWithTypes []

        context "first constructor has a leading pipe" $ do
            result <- parseFile "CtorOptionalPipe.whippet"
            it "has the expected constructors" $
                result `shouldSatisfy` hasCtorsLabelled ["True", "False"]

        context "single type parameter" $ do
            result <- parseFile "PhantomType.whippet"
            it "has the expected type parameter" $
                result `shouldSatisfy` typeHasTypeParams ["a"]

        context "multiple type parameters" $ do
            result <- parseFile "CoerceType.whippet"
            it "has the expected type parameters" $
                result `shouldSatisfy` absTypeHasTypeParams ["source", "dest"]

        context "constructor reference to type parameters" $ do
            result <- parseFile "Either.whippet"
            it "has the expected ctor parameter types" $
                result `shouldSatisfy` hasCtorParamsWithTypes ["e", "a"]
