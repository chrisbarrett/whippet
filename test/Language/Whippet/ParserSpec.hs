{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.ParserSpec where

import           Control.Lens
import           Control.Lens.Extras
import qualified Data.Either             as Either
import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import           Language.Whippet.AST
import qualified Language.Whippet.Parser as Parser
import qualified Paths_whippet           as Paths
import           Test.Hspec
import           Text.Trifecta
import           Text.Trifecta.Result

main :: IO ()
main = hspec spec

decls :: AST s -> [Decl s]
decls r =
    case r of
        AstModule _ _ ds    -> ds
        AstSignature _ _ ds -> ds
        AstType {}          -> error "No decls"
        AstRecordType {}    -> error "No decls"

parseFile s = runIO $ do
    file <- Paths.getDataFileName ("test/resources/" <> s)
    res <- Parser.parseFile file
    case res of
      Success x -> pure (Right x)
      Failure e -> pure (Left e)

hasIdentifier :: Text -> Either e (AST s) -> Bool
hasIdentifier s =
   (==) s . view (_Right.identLabel) . fmap extract
  where
    extract (AstModule _ i _)       = i
    extract (AstSignature _ i _)    = i
    extract (AstType _ i _ _)       = i
    extract (AstRecordType _ i _ _) = i

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
                view (_Right._AstType._4)

            hasCtorTypeParams :: [Text] -> Either e (AST s) -> Bool
            hasCtorTypeParams cs =
                (==) cs
                . fmap (view identLabel)
                . view (_Right._AstType._3)

            hasCtorsLabelled :: [Text] -> Either e (AST s) -> Bool
            hasCtorsLabelled cs =
                (==) cs
                . fmap (view (ctorIdent.identLabel))
                . ctorsFromAst

            hasCtorParamsNamed :: [Text] -> Either e (AST s) -> Bool
            hasCtorParamsNamed ps =
                (==) ps
                . map (view identLabel)
                . concatMap (view ctorParams)
                . ctorsFromAst

            fieldDeclsFromAst :: Either e (AST s) -> [FieldDecl s]
            fieldDeclsFromAst =
                view (_Right._AstRecordType._4)

            hasFieldDeclsLabelled :: [Text] -> Either e (AST s) -> Bool
            hasFieldDeclsLabelled ps =
                (==) ps
                . fmap (view (fieldDeclIdent.identLabel))
                . fieldDeclsFromAst

            hasFieldDeclTypesNamed :: [Text] -> Either e (AST s) -> Bool
            hasFieldDeclTypesNamed ps =
                (==) ps
                . map (view identLabel)
                . concatMap (view fieldDeclParams)
                . fieldDeclsFromAst

        context "abstract type" $ do
            result <- parseFile "3.whippet"
            it "returns a type declaration" $
                result `shouldSatisfy` is (_Right._AstType)
            it "has the expected identifier" $
                result `shouldSatisfy` hasIdentifier "Void"
            it "has no constructors" $
                result `shouldSatisfy` hasCtorsLabelled []

        context "nullary constructor" $ do
            result <- parseFile "4.whippet"
            it "returns a type declaration" $
                result `shouldSatisfy` is (_Right._AstType)
            it "has the expected identifier" $
                result `shouldSatisfy` hasIdentifier "Unit"
            it "has the expected constructor" $
                result `shouldSatisfy` hasCtorsLabelled ["Unit"]
            it "has no parameters" $
                result `shouldSatisfy` hasCtorParamsNamed []

        context "multiple nullary constructors" $ do
            result <- parseFile "5.whippet"
            it "returns a type declaration" $
                result `shouldSatisfy` is (_Right._AstType)
            it "has the expected identifier" $
                result `shouldSatisfy` hasIdentifier "Bool"
            it "has the expected constructor" $
                result `shouldSatisfy` hasCtorsLabelled ["True", "False"]
            it "has no parameters" $
                result `shouldSatisfy` hasCtorParamsNamed []

        context "single type parameter" $ do
            result <- parseFile "6.whippet"
            it "has the expected type parameter" $
                result `shouldSatisfy` hasCtorTypeParams ["a"]

        context "multiple type parameters" $ do
            result <- parseFile "7.whippet"
            it "has the expected type parameter" $
                result `shouldSatisfy` hasCtorTypeParams ["source", "dest"]

        context "record type" $ do
            result <- parseFile "8.whippet"
            it "returns a type declaration" $
                result `shouldSatisfy` is (_Right._AstRecordType)
            it "has the expected identifier" $
                result `shouldSatisfy` hasIdentifier "IntPair"
            it "has the expected fields" $
                result `shouldSatisfy` hasFieldDeclsLabelled ["fst", "snd"]
            it "has the expected field types" $
                result `shouldSatisfy` hasFieldDeclTypesNamed ["Int", "Int"]
