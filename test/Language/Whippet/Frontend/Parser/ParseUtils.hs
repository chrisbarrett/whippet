{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-partial-type-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Frontend.Parser.ParseUtils where

import           Control.Lens
import           Control.Monad                    (when)
import           Data.Monoid                      ((<>))
import Data.Text (Text)
import           Data.String                      (fromString)
import           Language.Whippet.Frontend.AST
import qualified Language.Whippet.Frontend.Parser as Parser
import qualified Paths_whippet                    as Paths
import           Test.Hspec
import           Text.PrettyPrint.ANSI.Leijen     (Doc)
import qualified Text.Trifecta                    as Trifecta
import qualified Text.Trifecta.Delta              as Trifecta

type ParsedAst = Either Doc AST

resultToEither :: Trifecta.Result a -> Either Doc a
resultToEither (Trifecta.Success x) = Right x
resultToEither (Trifecta.Failure e) = Left ("\n" <> e)

parseFileFromResources :: Trifecta.Parser a -> FilePath -> IO (Either Doc a)
parseFileFromResources parser name = do
    content <- loadResource name
    pure (resultToEither (parse content))
  where
    loadResource name = do
        realPath <- Paths.getDataFileName ("test/resources/" <> name)
        readFile realPath

    parse content =
        let delta = Trifecta.Directed (fromString name) 0 0 0 0
        in Trifecta.parseByteString parser delta (fromString content)

parseFile :: FilePath -> _ a (Either Doc AST)
parseFile name =
    runIO $ parseFileFromResources (Parser.ast <* Trifecta.eof) name

emptySpan :: Trifecta.Span
emptySpan = Trifecta.Span mempty mempty mempty

ident :: Text -> Ident
ident = Ident emptySpan
