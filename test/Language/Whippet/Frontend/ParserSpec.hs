{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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


fieldToText :: Field -> Text
fieldToText Field {..} =
    ident <> ": " <> typeToText _fieldType
  where
    ident = _identLabel _fieldIdent

typeToText :: Type -> Text

typeToText (TyNominal i) =
    _identLabel i

typeToText (TyApp x y) =
    Text.unwords [typeToText x, typeToText y]

typeToText (TyStructural fs) =
    "{" <> Text.intercalate ", " (map fieldToText fs) <> "}"

typeToText (TyFun a b) =
    "(" <> typeToText a <> " -> " <> typeToText b <> ")"

emptySpan :: Trifecta.Span
emptySpan = Trifecta.Span mempty mempty mempty

ident :: Text -> Ident
ident = Ident emptySpan

nominalType :: Text -> Type
nominalType = TyNominal . ident

int :: Integer -> Expr
int = ELit . LitInt

var :: Text -> Expr
var = EVar . Ident emptySpan


spec :: Spec
spec = do

    describe "parsing modules" $ do
        let body :: ParsedAst -> [AST]
            body ast =
                ast ^. _Right._AstModule._2

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


    describe "parsing signatures" $ do

        let identifiers :: ParsedAst -> [Ident]
            identifiers ast =
                ast ^.. _Right._AstSignature._2.traverse.identifier

            types :: ParsedAst -> [Text]
            types ast =
                ast ^.. _Right._AstSignature._2.traverse._DecFun._2.to typeToText

            decls :: ParsedAst -> [Decl]
            decls ast =
                ast ^. _Right._AstSignature._2

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
                    identifiers result `shouldBe` [ident "foo"]
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
                    identifiers result `shouldBe` [ident "foo", ident "bar"]
                it "has the expected types" $
                    types result `shouldBe` ["(A -> B)", "(B -> C)"]

        context "signature with abstract type" $ do
            result <- parseFile "SignatureWithAbsType.whippet"
            whenParsesToSignature result $
                it "has the expected type name" $
                    identifiers result `shouldBe` [ident "T"]

        context "realistic signature" $ do
            result <- parseFile "Option.whippet"
            whenParsesToSignature result $ do
                it "has the expected module name" $
                    result `shouldSatisfy` astHasIdentifier "Option"
                it "has the expected identifiers" $
                    identifiers result `shouldBe` [ ident "T"
                                                  , ident "some?"
                                                  , ident "none?"
                                                  , ident "get"
                                                  , ident "map"
                                                  , ident "filter"
                                                  ]


    -- Type declarations

    describe "parsing a record declaration" $ do
        let fieldLabels :: ParsedAst -> [Ident]
            fieldLabels ast =
                ast ^.. _Right._AstDecl._DecRecordType._3.traverse.identifier

            fieldTypes :: ParsedAst -> [Type]
            fieldTypes ast =
                ast ^.. _Right._AstDecl._DecRecordType._3.traverse.fieldType

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
                    fieldLabels result `shouldBe` [ident "fst", ident "snd"]
                it "has the expected field types" $
                    fieldTypes result `shouldBe` [nominalType "Int", nominalType "Int"]

        context "record type with type parameters" $ do
            result <- parseFile "Pair.whippet"
            whenParsesToRecordDecl result $ do
                it "has the expected fields" $
                    fieldLabels result `shouldBe` [ident "fst", ident "snd"]
                it "has the expected field types" $
                    fieldTypes result `shouldBe` [nominalType "a", nominalType "b"]

        context "record type with comma before first field" $ do
            result <- parseFile "RecordOptionalLeadingComma.whippet"
            whenParsesToRecordDecl result $
                it "has the expected fields" $
                    fieldLabels result `shouldBe` [ident "fst", ident "snd"]

    describe "parsing a type declaration" $ do
        let ctorsFromAst :: ParsedAst -> [Ctor]
            ctorsFromAst ast =
                ast ^. _Right._AstDecl._DecDataType._3

            typeParameters :: ParsedAst -> [Ident]
            typeParameters ast =
                ast ^.. _Right._AstDecl._DecDataType._2.traverse.identifier

            ctorLabels :: ParsedAst -> [Ident]
            ctorLabels ast =
                ast ^.. to ctorsFromAst.traverse.identifier

            ctorParamTypes :: ParsedAst -> [Ident]
            ctorParamTypes ast =
                ast ^.. to ctorsFromAst.traverse.ctorParams.traverse
                       .to typeIdentifiers._Just.each

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
                    ctorLabels result `shouldBe` [ident "Unit"]
                it "has no parameters" $
                    ctorParamTypes result `shouldSatisfy` is _Empty

        context "multiple nullary constructors" $ do
            result <- parseFile "Bool.whippet"
            whenParsesToTypeDecl result $ do
                it "has the expected constructors" $
                    ctorLabels result `shouldBe` [ident "True", ident "False"]
                it "has no parameters" $
                    ctorParamTypes result `shouldSatisfy` is _Empty

        context "first constructor has a leading pipe" $ do
            result <- parseFile "CtorOptionalPipe.whippet"
            whenParsesToTypeDecl result $
                it "has the expected constructors" $
                    ctorLabels result `shouldBe` [ident "True", ident "False"]

        context "single type parameter" $ do
            result <- parseFile "PhantomType.whippet"
            whenParsesToTypeDecl result $
                it "has the expected type parameter" $
                    typeParameters result `shouldBe` [ident "a"]

        context "multiple type parameters" $ do
            result <- parseFile "CoerceType.whippet"
            whenParsesToTypeDecl result $
                it "has the expected type parameters" $
                    typeParameters result `shouldBe` [ident "source", ident "dest"]

        context "constructor reference to type parameters" $ do
            result <- parseFile "Either.whippet"
            whenParsesToTypeDecl result $
                it "has the expected ctor parameter types" $
                    ctorParamTypes result `shouldBe` [ident "e", ident "a"]

    describe "parsing a function signature" $ do
        let ident :: ParsedAst -> Text
            ident ast =
                ast ^. _Right._AstDecl._DecFun._1.identifier.text

            fnType :: ParsedAst -> Text
            fnType ast =
                ast ^. _Right._AstDecl._DecFun._2.to typeToText

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
        parseExpr =
            resultToEither . Trifecta.parseByteString (Parser.expr <* Trifecta.eof) mempty

        parseExprFromFile file = runIO $
            parseFileFromResources (Parser.expr <* Trifecta.eof) file


    describe "variable references" $ do

        let whenParsesToVar result assertions = do
                it "parses to a variable reference" $
                  result `shouldSatisfy` is (_Right._EVar)
                when (is _Right result) assertions

            label :: Either Doc Expr -> Text
            label = view (_Right._EVar.identifier.text)

        context "identifier ending with an underscore" $ do
            let result = parseExpr "x_"
            whenParsesToVar result $
                it "has the expected identifier" $
                    label result `shouldBe` "x_"

        context "identifier starting with a question mark" $ do
            let result = parseExpr "?x"
            it "should fail to parse" $
                result `shouldSatisfy` is _Left

        context "identifier containing a question mark" $ do
            let result = parseExpr "x?"
            whenParsesToVar result $
                it "has the expected identifier" $
                    label result `shouldBe` "x?"

        context "identifier starting with a number" $ do
            let result = parseExpr "1x"
            it "should fail to parse" $
                result `shouldSatisfy` is _Left

        context "identifier containing a number" $ do
            let result = parseExpr "x1"
            whenParsesToVar result $
                it "has the expected identifier" $
                    label result `shouldBe` "x1"


    describe "holes" $ do

        let whenParsesToHole result assertions = do
                it "parses to a hole" $
                    result `shouldSatisfy` is (_Right._EHole)
                when (is _Right result) assertions

            holeContent =
                 view (_Right._EHole.text)

        context "underscore" $ do
            let result = parseExpr "_"
            whenParsesToHole result $
                it "has the expected identifier" $
                    holeContent result `shouldBe` "_"

        context "named hole" $ do
            let result = parseExpr "_1"
            whenParsesToHole result $
                it "has the expected identifier" $
                    holeContent result `shouldBe` "_1"


    let itIsInt result =
            it "parses to an integer" $
                result `shouldSatisfy` is (_Right._ELit._LitInt)

    describe "integer literals" $ do

        context "natural number" $ do
            let result = parseExpr "1"
            itIsInt result

        context "signed positive integer" $ do
            let result = parseExpr "+1"
            itIsInt result

        context "signed negative integer" $ do
            let result = parseExpr "-1"
            itIsInt result


    let itIsFloat result =
            it "parses to a float" $
                result `shouldSatisfy` is (_Right._ELit._LitScientific)

    describe "floating point literals" $ do

        context "unsigned float" $ do
            let result = parseExpr "1.5"
            itIsFloat result

        context "signed positive float" $ do
            let result = parseExpr "+1.5"
            itIsFloat result

        context "signed negative float" $ do
            let result = parseExpr "-1.5"
            itIsFloat result

    describe "scientific literals" $ do

        context "unsigned scientific" $ do
            let result = parseExpr "1e-6"
            itIsFloat result

        context "positive scientific" $ do
            let result = parseExpr "+1e-6"
            itIsFloat result

        context "negative scientific" $ do
            let result = parseExpr "-1e-6"
            itIsFloat result


    describe "string literal" $ do

        let whenParsesToString result assertions = do
                it "parses to a string" $
                    result `shouldSatisfy` is (_Right._ELit._LitString)
                when (is _Right result) assertions

            stringContent = view (_Right._ELit._LitString)

        context "simple string literal" $ do
            result <- parseExprFromFile "SimpleStringLiteral.whippet"
            whenParsesToString result $
                it "has the expected content" $
                    stringContent result `shouldBe` "foo"

        context "multiline string literal" $ do
            result <- parseExprFromFile "MultilineStringLiteral.whippet"
            whenParsesToString result $
                it "has the expected content" $
                    stringContent result `shouldBe` "foo\n bar"

        context "string literal with escape sequence" $ do
            result <- parseExprFromFile "StringLiteralWithEscapes.whippet"
            whenParsesToString result $
                it "has the expected content" $
                    stringContent result `shouldBe` "foo\nbar"


    describe "list literal" $ do

        let whenParsesToList result assertions = do
                it "parses to a list" $
                    result `shouldSatisfy` is (_Right._ELit._LitList)
                when (is _Right result) assertions

            listContent = view (_Right._ELit._LitList)

        context "empty list literal" $ do
            let result = parseExpr "[]"
            whenParsesToList result $
                it "has the expected content" $
                    listContent result `shouldSatisfy` is _Empty

        context "singleton list literal" $ do
            let result = parseExpr "[1]"
            whenParsesToList result $
                it "has the expected content" $
                    listContent result `shouldBe` [int 1]

        context "comma-separated list entries" $ do
            let result = parseExpr "[1,2]"
            whenParsesToList result $
                it "has the expected content" $
                    listContent result `shouldBe` [int 1, int 2]

        context "optional leading and trailing commas" $ do
            let result = parseExpr "[,1,2,]"
            whenParsesToList result $
                it "has the expected content" $
                    listContent result `shouldBe` [int 1, int 2]


    describe "record literal" $ do

        let whenParsesToRecord result assertions = do
                it "parses to a record" $
                    result `shouldSatisfy` is (_Right._ELit._LitRecord)
                when (is _Right result) assertions

            recordContent :: Either Doc Expr -> [(Ident, Expr)]
            recordContent ast =
                ast ^. _Right._ELit._LitRecord

        context "empty record" $ do
            let result = parseExpr "{}"
            whenParsesToRecord result $
                it "has the expected content" $
                    recordContent result `shouldSatisfy` is _Empty

        context "single field" $ do
            let result = parseExpr "{x:1}"
            whenParsesToRecord result $
                it "has the expected content" $
                    recordContent result `shouldBe` [(ident "x", int 1)]

        context "comma-separated fields" $ do
            let result = parseExpr "{x:1,y:2}"
            whenParsesToRecord result $
                it "has the expected content" $
                    recordContent result `shouldBe` [ (ident "x", int 1)
                                                    , (ident "y", int 2)
                                                    ]

        context "optional leading and trailing commas" $ do
            let result = parseExpr "{,x:1,y:2,}"
            whenParsesToRecord result $
                it "has the expected content" $
                    recordContent result `shouldBe` [ (ident "x", int 1)
                                                    , (ident "y", int 2)
                                                    ]


    describe "Type annotations" $ do

        let whenParsesToAnnotation result assertions = do
                it "parses to a type annotation" $
                    result `shouldSatisfy` is (_Right._EAnnotation)
                when (is _Right result) assertions

            annType :: Either Doc Expr -> Maybe Type
            annType (Right (EAnnotation _ t)) = Just t
            annType _                         = Nothing

        context "simple annotation" $ do
            let result = parseExpr "1 : Int"
            whenParsesToAnnotation result $
                it "has the expected type" $
                    annType result `shouldBe` Just (nominalType "Int")

        context "nested type annotations" $ do
            let result = parseExpr "1 : Int : Int"
            it "should be rejected" $
                result `shouldSatisfy` is _Left

    describe "if then else" $ do

        let whenParsesToIfThenElse result assertions = do
                it "parses to an if-then-else expression" $
                    result `shouldSatisfy` is (_Right._EIf)
                when (is _Right result) assertions

            subexpressions :: Either Doc Expr -> [Expr]
            subexpressions = view (_Right.to children)

        context "simple expressions " $ do
            let result = parseExpr "if foo then bar else baz"
            whenParsesToIfThenElse result $
                it "has the expected values" $
                    subexpressions result `shouldBe` [var "foo", var "bar", var "baz"]


    let dVar :: Text -> Discriminator
        dVar x = DVar (Ident emptySpan x)

        dAnn :: Discriminator -> Type -> Discriminator
        dAnn = DAnn

        dCtor :: Text -> Discriminator
        dCtor x = DCtor (Ident emptySpan x)

        dApp :: [Discriminator] -> Discriminator
        dApp = foldl1 DApp

        dRec :: [Discriminator] -> Discriminator
        dRec = DRec

        patAs :: Discriminator -> Discriminator -> Discriminator
        patAs = DAs

    describe "lambda" $ do

        let discriminators :: Either Doc Expr -> [Discriminator]
            discriminators expr =
                expr ^.. _Right._EFn.traverse.patDiscriminator

            bodyForms :: Either Doc Expr -> [Expr]
            bodyForms expr =
                expr ^.. _Right._EFn.traverse.patBody

        describe "bare lambda" $ do

            let whenParsesToLambda result assertions = do
                    it "parses to a lambda expression" $
                        result `shouldSatisfy` is (_Right._EFn)
                    when (is _Right result) assertions

                discriminator :: Either Doc Expr -> [Discriminator]
                discriminator expr =
                    expr ^.. _Right._EFn.traverse.patDiscriminator

                body :: Either Doc Expr -> [Expr]
                body expr =
                    expr ^.. _Right._EFn.traverse.patBody

            context "named binding" $ do
                let result = parseExpr "fn x -> 0"
                whenParsesToLambda result $ do
                    it "has the expected binder" $
                        discriminator result `shouldBe` [dVar "x"]
                    it "has the expected body" $
                        body result `shouldBe` [int 0]

            context "named binding with type annotation" $ do
                let result = parseExpr "fn x: Int -> 0"
                whenParsesToLambda result $ do
                    it "has the expected binder" $
                        discriminator result `shouldBe` [dVar "x" `dAnn` nominalType "Int"]
                    it "has the expected body" $
                        body result `shouldBe` [int 0]

            context "'as' pattern" $ do
                let result = parseExpr "fn u as Unit -> 1"
                whenParsesToLambda result $
                    it "has the expected binder" $
                        discriminators result `shouldBe` [dVar "u" `patAs` dCtor "Unit"]

            context "structural type" $ do
                let result = parseExpr "fn {fst, snd} -> fst "
                whenParsesToLambda result $ do
                    it "has the expected binder" $
                        discriminators result `shouldBe` [dRec [dVar "fst", dVar "snd"]]
                    it "has the expected body" $
                        bodyForms result `shouldBe` [var "fst"]


        describe "branching lambda" $ do

            let whenParsesToLambda result assertions = do
                    it "parses to a lambda expression" $
                        result `shouldSatisfy` is (_Right._EFn)
                    when (is _Right result) assertions

            context "single case" $ do
                let result = parseExpr "fn { x: Int -> 0 }"
                whenParsesToLambda result $ do
                    it "has the expected binder" $
                        discriminators result `shouldBe` [dVar "x" `dAnn` nominalType "Int"]
                    it "has the expected body" $
                        bodyForms result `shouldBe` [int 0]

            context "optional leading pipe" $ do
                let result = parseExpr "fn { | x -> 0 }"
                whenParsesToLambda result $
                    it "parses a single case" $
                        length (bodyForms result) `shouldBe` 1

            context "matching single nullary constructor" $ do
                let result = parseExpr "fn { Unit -> 0 }"
                whenParsesToLambda result $ do
                    it "has the expected binder" $
                        discriminators result `shouldBe` [dCtor "Unit"]
                    it "has the expected body" $
                        bodyForms result `shouldBe` [int 0]

            context "matching multiple nullary constructors" $ do
                let result = parseExpr "fn { True -> 0 | False -> 1 }"
                whenParsesToLambda result $ do
                    it "has the expected binder" $
                        discriminators result `shouldBe` [dCtor "True", dCtor "False"]
                    it "has the expected body" $
                        bodyForms result `shouldBe` [int 0, int 1]

            context "matching constructors with parameters" $ do
                let result = parseExpr "fn { None -> 0 | Some x -> 1 }"
                whenParsesToLambda result $
                    it "has the expected binders" $
                        discriminators result `shouldBe` [ dCtor "None"
                                                        , dApp [dCtor "Some", dVar "x"]
                                                        ]

            context "matching constructor with multiple parameters" $ do
                let result = parseExpr "fn { Pair x y  -> 1 }"
                whenParsesToLambda result $
                    it "has the expected binder" $
                        discriminators result `shouldBe` [dApp [ dCtor "Pair"
                                                              , dVar "x"
                                                              , dVar "y"
                                                              ]]

            context "'as' pattern" $ do
                let result = parseExpr "fn { u as Unit -> u }"
                whenParsesToLambda result $
                    it "has the expected binder" $
                        discriminators result `shouldBe` [dVar "u" `patAs` dCtor "Unit"]

            context "matching record type" $ do
                let result = parseExpr "fn { {fst, snd} -> fst }"
                whenParsesToLambda result $ do
                    it "has the expected binder" $
                        discriminators result `shouldBe` [dRec [dVar "fst", dVar "snd"]]
                    it "has the expected body" $
                        bodyForms result `shouldBe` [var "fst"]

            context "matching record type, leading comma" $ do
                let result = parseExpr "fn { {,fst} -> fst }"
                whenParsesToLambda result $
                    it "has the expected binder" $
                        discriminators result `shouldBe` [dRec [dVar "fst"]]

    describe "match expression" $ do

        let scrutinee :: Either Doc Expr -> [Expr]
            scrutinee expr =
                expr ^.. _Right._EMatch._1

            discriminators :: Either Doc Expr -> [Discriminator]
            discriminators expr =
                expr ^.. _Right._EMatch._2.traverse.patDiscriminator

            bodyForms :: Either Doc Expr -> [Expr]
            bodyForms expr =
                expr ^.. _Right._EMatch._2.traverse.patBody

            whenParsesToMatch result assertions = do
                it "parses to a match expression" $
                    result `shouldSatisfy` is (_Right._EMatch)
                when (is _Right result) assertions

        context "single case" $ do
            let result = parseExpr "match x { y -> 0 }"
            whenParsesToMatch result $ do
                it "has the expected scrutinee" $
                    scrutinee result `shouldBe` [var "x"]
                it "has the expected binder" $
                    discriminators result `shouldBe` [dVar "y"]
                it "has the expected body" $
                    bodyForms result `shouldBe` [int 0]

        context "matching multiple nullary constructors" $ do
            let result = parseExpr "match b { True -> 0 | False -> 1 }"
            whenParsesToMatch result $ do
                it "has the expected scrutinee" $
                    scrutinee result `shouldBe` [var "b"]
                it "has the expected binder" $
                    discriminators result `shouldBe` [dCtor "True", dCtor "False"]
                it "has the expected body" $
                    bodyForms result `shouldBe` [int 0, int 1]

        context "'as' pattern" $ do
            let result = parseExpr "match x { u as Unit -> 1 }"
            whenParsesToMatch result $
                it "has the expected binder" $
                    discriminators result `shouldBe` [dVar "u" `patAs` dCtor "Unit"]

        context "matching record type" $ do
            let result = parseExpr "match t { {fst, snd} -> fst }"
            whenParsesToMatch result $ do
                it "has the expected scrutinee" $
                    scrutinee result `shouldBe` [var "t"]
                it "has the expected binder" $
                    discriminators result `shouldBe` [dRec [dVar "fst", dVar "snd"]]
                it "has the expected body" $
                    bodyForms result `shouldBe` [var "fst"]

        context "matching record type, leading comma" $ do
            let result = parseExpr "match p { {,fst} -> fst }"
            whenParsesToMatch result $ do
                it "has the expected scrutinee" $
                    scrutinee result `shouldBe` [var "p"]
                it "has the expected binder" $
                    discriminators result `shouldBe` [dRec [dVar "fst"]]
