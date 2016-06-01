{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Frontend.ParserSpec where

import           Control.Lens
import           Control.Lens.Extras
import           Control.Monad                    (when)
import qualified Data.ByteString.Internal         as BS
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

type ParsedAst = Either Doc AST

main :: IO ()
main = hspec spec

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

parseFile name =
    runIO $ parseFileFromResources (Parser.ast <* Trifecta.eof) name

astHasIdentifier :: Text -> ParsedAst -> Bool
astHasIdentifier s =
   (==) s . view (_Right.identifier.text)

spec :: Spec
spec = do

    -- Modules

    describe "parsing modules" $ do
        let body :: ParsedAst -> [AST]
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

        let identifiers :: ParsedAst -> [Text]
            identifiers =
                fmap (view (identifier.text)) . view (_Right._AstSignature._2)

            types :: ParsedAst -> [Text]
            types =
                fmap (view (_DecFun._2.to typeToText))
                . view (_Right._AstSignature._2)

            decls :: ParsedAst -> [Decl]
            decls = view (_Right._AstSignature._2)

            declsCount :: ParsedAst -> Int
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

        context "realistic signature" $ do
            result <- parseFile "Option.whippet"
            whenParsesToSignature result $ do
                it "has the expected module name" $
                    result `shouldSatisfy` astHasIdentifier "Option"
                it "has the expected identifiers" $
                    identifiers result `shouldBe` [ "T"
                                                  , "some?"
                                                  , "none?"
                                                  , "get"
                                                  , "map"
                                                  , "filter"
                                                  ]


    -- Type declarations

    describe "parsing a record declaration" $ do
        let fieldsFromAst :: ParsedAst -> [Field]
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
        let ctorsFromAst :: ParsedAst -> [Ctor]
            ctorsFromAst =
                view (_Right._AstDecl._DecDataType._3)

            typeParameters :: ParsedAst -> [Text]
            typeParameters =
                fmap (view (identifier.text))
                . view (_Right._AstDecl._DecDataType._2)

            ctorLabels :: ParsedAst -> [Text]
            ctorLabels =
                fmap (view (identifier.text)) . ctorsFromAst

            ctorParamTypes :: ParsedAst -> [Text]
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
        let ident :: ParsedAst -> Text
            ident = view (_Right._AstDecl._DecFun._1.identifier.text)

            fnType :: ParsedAst -> Text
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

    let parseExpr :: BS.ByteString -> Either Doc Expr
        parseExpr = resultToEither . Trifecta.parseByteString (Parser.expr <* Trifecta.eof) mempty

        parseExprFromFile file = runIO $
            parseFileFromResources (Parser.expr <* Trifecta.eof) file

    describe "variable references" $ do

        let whenParsesToVar result assertions = do
                it "parses to a variable reference" $
                  result `shouldSatisfy` is (_Right._EVar)
                when (is _Right result) assertions

            ident :: Either Doc Expr -> Text
            ident = view (_Right._EVar.identifier.text)

        context "identifier ending with an underscore" $ do
            let result = parseExpr "x_"
            whenParsesToVar result $
                it "has the expected identifier" $
                    ident result `shouldBe` "x_"

        context "identifier starting with a question mark" $ do
            let result = parseExpr "?x"
            it "should fail to parse" $
                result `shouldSatisfy` is _Left

        context "identifier containing a question mark" $ do
            let result = parseExpr "x?"
            whenParsesToVar result $
                it "has the expected identifier" $
                    ident result `shouldBe` "x?"

        context "identifier starting with a number" $ do
            let result = parseExpr "1x"
            it "should fail to parse" $
                result `shouldSatisfy` is _Left

        context "identifier containing a number" $ do
            let result = parseExpr "x1"
            whenParsesToVar result $
                it "has the expected identifier" $
                    ident result `shouldBe` "x1"

    describe "holes" $ do

        let whenParsesToHole result assertions = do
                it "parses to a hole" $
                    result `shouldSatisfy` is (_Right._EHole)
                when (is _Right result) assertions

            holeContent =
                 view (_Right._EHole.text)

        context "underscore" $ do
            let result = parseExpr "_"
            whenParsesToHole result $ do
                it "has the expected identifier" $
                    holeContent result `shouldBe` "_"

        context "named hole" $ do
            let result = parseExpr "_1"
            whenParsesToHole result $ do
                it "has the expected identifier" $
                    holeContent result `shouldBe` "_1"

    describe "integer literals" $ do

        context "natural number" $ do
            let result = parseExpr "1"
            it "parses to an integer" $
                result `shouldSatisfy` is (_Right._ELit._LitInt)

        context "signed positive integer" $ do
            let result = parseExpr "+1"
            it "parses to an integer" $
                result `shouldSatisfy` is (_Right._ELit._LitInt)

        context "signed negative integer" $ do
            let result = parseExpr "-1"
            it "parses to an integer" $
                result `shouldSatisfy` is (_Right._ELit._LitInt)

    describe "floating point literals" $ do

        context "unsigned float" $ do
            let result = parseExpr "1.5"
            it "parses to a float number" $
                result `shouldSatisfy` is (_Right._ELit._LitScientific)

        context "signed positive float" $ do
            let result = parseExpr "+1.5"
            it "parses to a float number" $
                result `shouldSatisfy` is (_Right._ELit._LitScientific)

        context "signed negative float" $ do
            let result = parseExpr "-1.5"
            it "parses to a float number" $
                result `shouldSatisfy` is (_Right._ELit._LitScientific)

    describe "scientific literals" $ do

        context "unsigned scientific" $ do
            let result = parseExpr "1e-6"
            it "parses to a float" $
                result `shouldSatisfy` is (_Right._ELit._LitScientific)

        context "positive scientific" $ do
            let result = parseExpr "+1e-6"
            it "parses to a float" $
                result `shouldSatisfy` is (_Right._ELit._LitScientific)

        context "negative scientific" $ do
            let result = parseExpr "-1e-6"
            it "parses to a float" $
                result `shouldSatisfy` is (_Right._ELit._LitScientific)

    describe "string literal" $ do

        let whenParsesToString result assertions = do
                it "parses to a string" $
                    result `shouldSatisfy` is (_Right._ELit._LitString)
                when (is _Right result) assertions

            stringContent = view (_Right._ELit._LitString)

        context "simple string literal" $ do
            result <- parseExprFromFile "SimpleStringLiteral.whippet"
            whenParsesToString result $ do
                it "has the expected content" $
                    stringContent result `shouldBe` "foo"

        context "multiline string literal" $ do
            result <- parseExprFromFile "MultilineStringLiteral.whippet"
            whenParsesToString result $ do
                it "has the expected content" $
                    stringContent result `shouldBe` "foo\n bar"

        context "string literal with escape sequence" $ do
            result <- parseExprFromFile "StringLiteralWithEscapes.whippet"
            whenParsesToString result $ do
                it "has the expected content" $
                    stringContent result `shouldBe` "foo\nbar"
