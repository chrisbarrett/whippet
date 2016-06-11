{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-partial-type-signatures #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Frontend.Parser.ParseUtils where

import           Control.Lens
import           Control.Monad                    (when)
import           Data.ByteString.Internal (ByteString)
import           Data.Monoid                      ((<>))
import           Data.String                      (fromString)
import           Data.Text (Text)
import           Language.Whippet.Frontend.AST
import qualified Language.Whippet.Frontend.Parser as Parser
import           Language.Whippet.Frontend.PPrint
import qualified Paths_whippet                    as Paths
import           Test.Hspec
import           Text.PrettyPrint.ANSI.Leijen     (Doc)
import qualified Text.Trifecta                    as Trifecta
import qualified Text.Trifecta.Delta              as Trifecta

type ParsedAst = Either Doc AST

resultToEither :: Trifecta.Result a -> Either Doc a
resultToEither (Trifecta.Success x) = Right x
resultToEither (Trifecta.Failure e) = Left ("\n" <> e)

parseString :: Parser.P a -> ByteString -> Either Doc a
parseString p =
  resultToEither .
  Trifecta.parseByteString (Parser.runP p <* Trifecta.eof) mempty

parseFileFromResources :: Parser.P a -> FilePath -> IO (Either Doc a)
parseFileFromResources parser name = do
    content <- loadResource name
    pure (resultToEither (parse content))
  where
    loadResource name = do
        realPath <- Paths.getDataFileName ("test/resources/" <> name)
        readFile realPath

    parse content =
        let delta = Trifecta.Directed (fromString name) 0 0 0 0
        in Trifecta.parseByteString (Parser.runP parser) delta (fromString content)

parseFile :: FilePath -> _ a (Either Doc AST)
parseFile name = do
    res <- runIO $ parseFileFromResources Parser.topLevel name
    pure $ case res of
              Left e         -> Left e
              Right []       -> Left "empty parse result"
              Right (x : xs) -> Right x

emptySpan :: Trifecta.Span
emptySpan = Trifecta.Span mempty mempty mempty

ident :: Text -> Ident
ident = Ident emptySpan

nominalType :: Text -> Type
nominalType = TyNominal . QualId . pure . ident

tyVar :: Text -> Type
tyVar = TyVar . ident

int :: Integer -> Expr
int = ELit . LitInt

var :: Text -> Expr
var = EVar . Ident emptySpan

eapp :: Expr -> Expr -> Expr
eapp x y = EApp (App x y)

getIdent :: Decl -> Maybe Text
getIdent (DecFun d)        = Just (d ^. functionIdent.pprint')
getIdent (DecFunSig d)     = Just (d ^. functionSigIdent.pprint')
getIdent (DecAbsType d)    = Just (d ^. absTypeIdent.pprint')
getIdent (DecDataType d)   = Just (d ^. dataTypeIdent.pprint')
getIdent (DecRecordType d) = Just (d ^. recordTypeIdent.pprint')
getIdent (DecTypeclass d)  = Just (d ^. typeclassIdent.pprint')
getIdent (DecInstance d)   = Just (d ^. instanceIdent.pprint')
