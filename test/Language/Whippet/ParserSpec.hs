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

identifier :: Functor f => f (AST s) -> f Text
identifier =
    fmap (view identText . extract)
  where
    extract (AstModule _ i _)    = i
    extract (AstSignature _ i _) = i
    extract (AstType _ i _)      = i

decls :: Monad m => m (AST s) -> m [Decl s]
decls r =
    r >>= \case
        AstModule _ _ ds    -> pure ds
        AstSignature _ _ ds -> pure ds
        AstType {} -> fail "No decls"

parseFile s = runIO $ do
    file <- Paths.getDataFileName ("test/resources/" <> s)
    res <- Parser.parseFile file
    case res of
      Success x -> pure (Right x)
      Failure e -> pure (Left e)

shouldParseIdent r s =
    either (const (fail "Parse failure")) (`shouldBe` s) r

spec :: Spec
spec = do

    -- Modules

    describe "parsing an empty module" $ do
        result <- parseFile "1.whippet"
        it "returns a module" $
            result `shouldSatisfy` is (_Right._AstModule)
        it "has the expected identifier" $
            identifier result `shouldParseIdent` "ExampleModule"
        it "has an empty body" $
            decls result `shouldSatisfy` is (_Right._Empty)

    -- Signatures

    describe "parsing an empty signature" $ do
        result <- parseFile "2.whippet"
        it "returns a signature" $
            result `shouldSatisfy` is (_Right._AstSignature)
        it "has the expected identifier" $
            identifier result `shouldParseIdent` "ExampleSignature"
        it "has an empty body" $
            decls result `shouldSatisfy` is (_Right._Empty)

    -- Type declarations

    describe "parsing a type declaration" $ do

        let constructors :: Functor f => f (AST s) -> f [Ctor s]
            constructors = fmap extract
              where
                extract (AstType _ _ ps) = ps
                extract _ = fail "Not a constructor"

            ctorLabels :: Functor f => f [Ctor s] -> f [Text]
            ctorLabels = (fmap.fmap) (view (ctorIdent.identText))

            shouldHaveConstructors r xs =
                (ctorLabels . constructors) r `shouldSatisfy` ((==) xs . view _Right)

        context "abstract type" $ do
            result <- parseFile "3.whippet"
            it "returns a type declaration" $
                result `shouldSatisfy` is (_Right._AstType)
            it "has the expected identifier" $
                identifier result `shouldParseIdent` "Void"

        context "nullary constructor" $ do
            result <- parseFile "4.whippet"
            it "returns a type declaration" $
                result `shouldSatisfy` is (_Right._AstType)
            it "has the expected identifier" $
                identifier result `shouldParseIdent` "Unit"
            it "has the expected constructor" $
                result `shouldHaveConstructors` ["Unit"]
            it "has no parameters" $ do
                let parameters = concatMap (view ctorParams) . view _Right
                constructors result `shouldSatisfy` (is _Empty . parameters)
