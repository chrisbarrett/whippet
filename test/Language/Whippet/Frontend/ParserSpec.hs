{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Frontend.ParserSpec where

import           Control.Lens
import           Control.Lens.Extras
import           Control.Monad                    (when)
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

parseFile name = do
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
        in Trifecta.parseByteString (Parser.ast <* Trifecta.eof) delta (fromString content)

astHasIdentifier :: Text -> ParsedAst s -> Bool
astHasIdentifier s =
   (==) s . view (_Right.identifier.text)

spec :: Spec
spec = do

    -- Modules

    describe "parsing modules" $ do
        let body :: ParsedAst s -> [AST s]
            body = view (_Right._AstModule._2)

            whenParsesToModule result assertions = do
                it "parses to a module" $
                  result `shouldSatisfy` is (_Right._AstModule)
                when (is _Right result) assertions

        context "empty module" $ do
            result <- parseFile "EmptyModule.whippet"
            whenParsesToModule result $ do
                it "has the expected identifier" $
                    result `shouldSatisfy` astHasIdentifier "ExampleModule"
                it "has an empty body" $
                    body result `shouldSatisfy` is _Empty

    -- Signatures

    describe "parsing signatures" $ do

        let identifiers :: ParsedAst s -> [Text]
            identifiers =
                fmap (view (identifier.text)) . view (_Right._AstSignature._2)

            types :: ParsedAst s -> [Text]
            types =
                fmap (view (_DecFun._2.to typeToText))
                . view (_Right._AstSignature._2)

            decls :: ParsedAst s -> [Decl s]
            decls = view (_Right._AstSignature._2)

            declsCount :: ParsedAst s -> Int
            declsCount = length . decls

            whenParsesToSignature result assertions = do
                it "parses to a signature" $
                  result `shouldSatisfy` is (_Right._AstSignature)
                when (is _Right result) assertions


        context "empty signature" $ do
            result <- parseFile "EmptySignature.whippet"
            whenParsesToSignature result $ do
                it "has the expected identifier" $
                    result `shouldSatisfy` astHasIdentifier "ExampleSignature"
                it "has an empty body" $
                    decls result `shouldSatisfy` is _Empty

        context "signature with function decl" $ do
            result <- parseFile "SignatureWithFn.whippet"
            whenParsesToSignature result $ do
                it "has the expected fn name" $
                    identifiers result `shouldBe` ["foo"]
                it "has one inner declaration" $
                    declsCount result `shouldBe` 1
                it "has the expected types" $
                    types result `shouldBe` ["(A -> B)"]

        context "signature with multiple function decls" $ do
            result <- parseFile "SignatureWithMultipleFns.whippet"
            whenParsesToSignature result $ do
                it "has two inner declarations" $
                    declsCount result `shouldBe` 2
                it "has the expected fn names" $
                    identifiers result `shouldBe` ["foo", "bar"]
                it "has the expected types" $
                    types result `shouldBe` ["(A -> B)", "(B -> C)"]

        context "signature with abstract type" $ do
            result <- parseFile "SignatureWithAbsType.whippet"
            whenParsesToSignature result $
                it "has the expected type name" $
                    identifiers result `shouldBe` ["T"]

        -- context "realistic signature" $ do
        --     result <- parseFile "Option.whippet"
        --     whenParsesToSignature result


    -- Type declarations

    describe "parsing a record declaration" $ do
        let fieldsFromAst :: ParsedAst s -> [Field s]
            fieldsFromAst =
                view (_Right._AstDecl._DecRecordType._3)

            fieldLabels =
                fmap (view (identifier.text)) . fieldsFromAst

            fieldTypeNames =
                fmap (view (fieldType.to typeIdentifiers._Just.each.text)) . fieldsFromAst

            whenParsesToRecordDecl result assertions = do
                it "parses to a record decl" $
                    result `shouldSatisfy` is (_Right._AstDecl._DecRecordType)
                when (is _Right result) assertions


        context "record type" $ do
            result <- parseFile "IntPair.whippet"
            whenParsesToRecordDecl result $ do
                it "has the expected identifier" $
                    result `shouldSatisfy` astHasIdentifier "IntPair"
                it "has the expected fields" $
                    fieldLabels result `shouldBe` ["fst", "snd"]
                it "has the expected field types" $
                    fieldTypeNames result `shouldBe` ["Int", "Int"]

        context "record type with type parameters" $ do
            result <- parseFile "Pair.whippet"
            whenParsesToRecordDecl result $ do
                it "has the expected fields" $
                    fieldLabels result `shouldBe` ["fst", "snd"]
                it "has the expected field types" $
                    fieldTypeNames result `shouldBe` ["a", "b"]

        context "record type with comma before first field" $ do
            result <- parseFile "RecordOptionalLeadingComma.whippet"
            whenParsesToRecordDecl result $
                it "has the expected fields" $
                    fieldLabels result `shouldBe` ["fst", "snd"]

    describe "parsing a type declaration" $ do
        let ctorsFromAst :: ParsedAst s -> [Ctor s]
            ctorsFromAst =
                view (_Right._AstDecl._DecDataType._3)

            typeParameters :: ParsedAst s -> [Text]
            typeParameters =
                fmap (view (identifier.text))
                . view (_Right._AstDecl._DecDataType._2)

            ctorLabels :: ParsedAst s -> [Text]
            ctorLabels =
                fmap (view (identifier.text)) . ctorsFromAst

            ctorParamTypes :: ParsedAst s -> [Text]
            ctorParamTypes =
                fmap (view (to typeIdentifiers._Just.each.text))
                . concatMap (view ctorParams)
                . ctorsFromAst

            whenParsesToTypeDecl result assertions = do
                it "parses to a type decl" $
                    result `shouldSatisfy` is (_Right._AstDecl._DecDataType)
                when (is _Right result) assertions

            whenParsesToAbsTypeDecl result assertions = do
                it "parses to a type decl" $
                    result `shouldSatisfy` is (_Right._AstDecl._DecAbsType)
                when (is _Right result) assertions


        context "abstract type" $ do
            result <- parseFile "Void.whippet"
            whenParsesToAbsTypeDecl result $
                it "has the expected identifier" $
                    result `shouldSatisfy` astHasIdentifier "Void"

        context "nullary constructor" $ do
            result <- parseFile "Unit.whippet"
            whenParsesToTypeDecl result $ do
                it "has the expected constructor" $
                    ctorLabels result `shouldBe` ["Unit"]
                it "has no parameters" $
                    ctorParamTypes result `shouldBe` []

        context "multiple nullary constructors" $ do
            result <- parseFile "Bool.whippet"
            whenParsesToTypeDecl result $ do
                it "has the expected constructors" $
                    ctorLabels result `shouldBe` ["True", "False"]
                it "has no parameters" $
                    ctorParamTypes result `shouldBe` []

        context "first constructor has a leading pipe" $ do
            result <- parseFile "CtorOptionalPipe.whippet"
            whenParsesToTypeDecl result $
                it "has the expected constructors" $
                    ctorLabels result `shouldBe` ["True", "False"]

        context "single type parameter" $ do
            result <- parseFile "PhantomType.whippet"
            whenParsesToTypeDecl result $
                it "has the expected type parameter" $
                    typeParameters result `shouldBe` ["a"]

        context "multiple type parameters" $ do
            result <- parseFile "CoerceType.whippet"
            whenParsesToTypeDecl result $
                it "has the expected type parameters" $
                    typeParameters result `shouldBe` ["source", "dest"]

        context "constructor reference to type parameters" $ do
            result <- parseFile "Either.whippet"
            whenParsesToTypeDecl result $
                it "has the expected ctor parameter types" $
                    ctorParamTypes result `shouldBe` ["e", "a"]

    describe "parsing a function signature" $ do
        let ident :: ParsedAst s -> Text
            ident = view (_Right._AstDecl._DecFun._1.identifier.text)

            fnType :: ParsedAst s -> Text
            fnType =
                view (_Right._AstDecl._DecFun._2.to typeToText)

            whenParsesToSigWithFn result assertions = do
                it "parses to a function signature" $
                    result `shouldSatisfy` is (_Right._AstDecl._DecFun)
                when (is _Right result) assertions

        context "unary type signature" $ do
            result <- parseFile "UnitFunSig.whippet"
            whenParsesToSigWithFn result $ do
                it "has the expected identifier" $
                    ident result `shouldBe` "unit"
                it "has the expected type parameters" $
                    fnType result `shouldBe` "Unit"

        context "binary type signature" $ do
            result <- parseFile "IdentityFunSig.whippet"
            whenParsesToSigWithFn result $ do
                it "has the expected identifier" $
                    ident result `shouldBe` "identity"
                it "has the expected type parameters" $
                    fnType result `shouldBe` "(a -> a)"

        context "ternary type signature" $ do
            result <- parseFile "ConstFunSig.whippet"
            whenParsesToSigWithFn result $ do
                it "has the expected identifier" $
                    ident result `shouldBe` "const"
                it "has the expected type parameters" $
                    fnType result `shouldBe` "(a -> (b -> a))"

        context "type signature with paranthesised identifier" $ do
            result <- parseFile "FunctionTyParens.whippet"
            whenParsesToSigWithFn result $ do
                it "has the expected identifier" $
                    ident result `shouldBe` "const"
                it "has the expected type parameters" $
                    fnType result `shouldBe` "(a -> (b -> a))"

        context "type signature with type constructor parameter" $ do
            result <- parseFile "FunctionTyCtor.whippet"
            whenParsesToSigWithFn result $ do
                it "has the expected identifier" $
                    ident result `shouldBe` "getOpt"
                it "has the expected type parameters" $
                    fnType result `shouldBe` "(a -> (Option a -> a))"

        context "type signature with function type parameter" $ do
            result <- parseFile "ListMapFun.whippet"
            whenParsesToSigWithFn result $ do
                it "has the expected identifier" $
                    ident result `shouldBe` "map"
                it "has the expected type parameters" $
                    fnType result `shouldBe` "((a -> b) -> (List a -> List b))"

        context "type signature with structural type as input" $ do
            result <- parseFile "StructuralTypeParameterInput.whippet"
            whenParsesToSigWithFn result $ do
                it "has the expected identifier" $
                    ident result `shouldBe` "first"
                it "has the expected type parameters" $
                    fnType result `shouldBe` "({fst: A, snd: B} -> A)"

        context "type signature with structural type as output" $ do
            result <- parseFile "StructuralTypeParameterOutput.whippet"
            whenParsesToSigWithFn result $ do
                it "has the expected identifier" $
                    ident result `shouldBe` "box"
                it "has the expected type parameters" $
                    fnType result `shouldBe` "(A -> {unpack: A})"
