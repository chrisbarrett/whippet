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
    fmap $ \case
        AstModule _ (Ident i) _ -> i
        AstSignature _ (Ident i) _ -> i


decls :: Functor f => f (AST s) -> f [Decl s]
decls =
    fmap $ \case
        AstModule _ _ ds -> ds
        AstSignature _ _ ds -> ds

parseFile s = runIO $ do
    file <- Paths.getDataFileName ("test/resources/" <> s)
    Parser.parseFile file

shouldParseIdent r s =
    r `shouldSatisfy` ((s ==) . view _Success)

spec :: Spec
spec = do

    -- Modules

    describe "parsing an empty module" $ do
        result <- parseFile "1.whippet"
        it "returns a module" $
            result `shouldSatisfy` is (_Success._AstModule)
        it "has the expected identifier" $
            identifier result `shouldParseIdent` "ExampleModule"
        it "has an empty body" $
            decls result `shouldSatisfy` is (_Success._Empty)

    -- Signatures

    describe "parsing an empty signature" $ do
        result <- parseFile "2.whippet"
        it "returns a signature" $
            result `shouldSatisfy` is (_Success._AstSignature)
        it "has the expected identifier" $
            identifier result `shouldParseIdent` "ExampleSignature"
        it "has an empty body" $
            decls result `shouldSatisfy` is (_Success._Empty)
