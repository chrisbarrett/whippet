module Language.Whippet.ParserSpec where

import           Language.Whippet.Parser
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "parsing an empty signature" $ do
        let sig ""
        it "returns an empty Sig" $
