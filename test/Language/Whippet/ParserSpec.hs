{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.ParserSpec where

import           Control.Lens            (( # ))
import           Control.Lens.Prism
import qualified Data.Either             as Either
import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import           Language.Whippet.AST
import qualified Language.Whippet.Parser as Parser
import qualified Paths_whippet           as Paths
import           Test.Hspec
import qualified Text.Trifecta.Result    as Result

main :: IO ()
main = hspec spec

identifier :: Either e (AST s) -> Maybe Text
identifier (Right (AstModule _ (Ident i) _)) = Just i
identifier _ = Nothing

decls :: Either e (AST s) -> Maybe [Decl s]
decls (Right (AstModule _ _ ds)) = Just ds
decls _ = Nothing

spec :: Spec
spec =
    describe "parsing an empty module" $ do
        result <- parseFile "1.whippet"
        it "returns a module" $
            result `shouldParseSatisfying` \ AstModule {} -> True
        it "has the expected identifier" $
            identifier result `shouldBe` Just "ExampleModule"
        it "has an empty body" $
            decls result `shouldBe` Just []

type ParsedAST = Either PP.Doc (AST Span)

shouldParseSatisfying :: ParsedAST -> (AST Span -> Bool) -> Expectation
shouldParseSatisfying r p =
    case r of
      Result.Failure {} -> False
      Result.Success r -> p r

parseFile s = do
    file <- runIO $ Paths.getDataFileName ("test/resources/" <> s)
    runIO $ Parser.parseFile file
