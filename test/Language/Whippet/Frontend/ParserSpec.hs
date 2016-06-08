{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Language.Whippet.Frontend.ParserSpec where

import           Control.Lens
import           Control.Lens.Extras
import           Control.Monad                           (when)
import qualified Data.ByteString.Internal                as BS
import           Data.List.NonEmpty                      (NonEmpty)
import qualified Data.List.NonEmpty                      as NonEmpty
import           Data.Monoid                             ((<>))
import           Data.String                             (fromString)
import           Data.Text                               (Text)
import qualified Data.Text                               as Text
import           Debug.Trace
import           Language.Whippet.Frontend.AST
import           Language.Whippet.Frontend.HasIdentifier
import qualified Language.Whippet.Frontend.Parser        as Parser
import qualified Paths_whippet                           as Paths
import           System.FilePath.Posix                   as FilePath
import           Test.Hspec
import           Text.PrettyPrint.ANSI.Leijen            (Doc)
import qualified Text.Trifecta                           as Trifecta
import qualified Text.Trifecta.Delta                     as Trifecta
import qualified Text.Trifecta.Result                    as Trifecta

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

fieldToText :: Field -> Text
fieldToText Field {..} =
    ident <> ": " <> typeToText _fieldType
  where
    ident = _identLabel _fieldIdent

typeToText :: Type -> Text

typeToText (TyNominal i) =
    _identLabel i

typeToText (TyVar i) =
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

tyVar :: Text -> Type
tyVar = TyVar . ident

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

        describe "empty module" $ do
            result <- parseFile "EmptyModule.whippet"
            whenParsesToModule result $ do
                it "has the expected identifier" $
                    result ^.. _Right.identifier `shouldBe` [ident "ExampleModule"]
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

        describe "empty signature" $ do
            result <- parseFile "EmptySignature.whippet"
            whenParsesToSignature result $ do
                it "has the expected identifier" $
                    result ^.. _Right.identifier `shouldBe` [ident "ExampleSignature"]
                it "has an empty body" $
                    decls result `shouldSatisfy` is _Empty

        describe "signature with function decl" $ do
            result <- parseFile "SignatureWithFn.whippet"
            whenParsesToSignature result $ do
                it "has the expected fn name" $
                    identifiers result `shouldBe` [ident "foo"]
                it "has one inner declaration" $
                    declsCount result `shouldBe` 1
                it "has the expected types" $
                    types result `shouldBe` ["(A -> B)"]

        describe "signature with multiple function decls" $ do
            result <- parseFile "SignatureWithMultipleFns.whippet"
            whenParsesToSignature result $ do
                it "has two inner declarations" $
                    declsCount result `shouldBe` 2
                it "has the expected fn names" $
                    identifiers result `shouldBe` [ident "foo", ident "bar"]
                it "has the expected types" $
                    types result `shouldBe` ["(A -> B)", "(B -> C)"]

        describe "signature with abstract type" $ do
            result <- parseFile "SignatureWithAbsType.whippet"
            whenParsesToSignature result $
                it "has the expected type name" $
                    identifiers result `shouldBe` [ident "T"]

        describe "realistic signature" $ do
            result <- parseFile "Option.whippet"
            whenParsesToSignature result $ do
                it "has the expected module name" $
                    result ^.. _Right.identifier `shouldBe` [ident "Option"]
                it "has the expected identifiers" $
                    identifiers result `shouldBe` [ ident "T"
                                                  , ident "some?"
                                                  , ident "none?"
                                                  , ident "get"
                                                  , ident "map"
                                                  , ident "filter"
                                                  ]


    describe "parsing typeclass declarations" $ do
        let body :: ParsedAst -> [Decl]
            body ast =
                ast ^. _Right._AstTypeclass._2

            name :: ParsedAst -> [Ident]
            name ast =
                ast ^.. _Right._AstTypeclass._1

            declarations :: ParsedAst -> [Ident]
            declarations ast =
                ast ^.. _Right._AstTypeclass._2.traverse.identifier

            whenParsesToTypeclass result assertions = do
                it "parses to a typeclass" $
                  result `shouldSatisfy` is (_Right._AstTypeclass)
                when (is _Right result) assertions

        describe "empty typeclass" $ do
            result <- parseFile "EmptyTypeclass.whippet"
            whenParsesToTypeclass result $ do
                it "has the expected identifier" $
                    name result `shouldBe` [ident "EmptyTypeclass"]
                it "has an empty body" $
                    body result `shouldSatisfy` is _Empty

        describe "typeclass with functions" $ do
            result <- parseFile "TypeclassWithFunctions.whippet"
            whenParsesToTypeclass result $
                it "has the expected declarations" $
                    declarations result `shouldBe` [ident "foo", ident "bar"]


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

        describe "record type" $ do
            result <- parseFile "IntPair.whippet"
            whenParsesToRecordDecl result $ do
                it "has the expected identifier" $
                    result ^.. _Right.identifier `shouldBe` [ident "IntPair"]
                it "has the expected fields" $
                    fieldLabels result `shouldBe` [ident "fst", ident "snd"]
                it "has the expected field types" $
                    fieldTypes result `shouldBe` [nominalType "Int", nominalType "Int"]

        describe "record type with type parameters" $ do
            result <- parseFile "Pair.whippet"
            whenParsesToRecordDecl result $ do
                it "has the expected fields" $
                    fieldLabels result `shouldBe` [ident "fst", ident "snd"]
                it "has the expected field types" $
                    fieldTypes result `shouldBe` [tyVar "a", tyVar "b"]

        describe "record type with comma before first field" $ do
            result <- parseFile "RecordOptionalLeadingComma.whippet"
            whenParsesToRecordDecl result $
                it "has the expected fields" $
                    fieldLabels result `shouldBe` [ident "fst", ident "snd"]

    describe "parsing a type declaration" $ do
        let ctorsFromAst :: ParsedAst -> [Ctor]
            ctorsFromAst ast =
                ast ^. _Right._AstDecl._DecDataType._3

            typeName :: ParsedAst -> [Ident]
            typeName ast =
                ast ^.. _Right.identifier

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

            typeIdentifiers :: Type -> Maybe [Ident]
            typeIdentifiers (TyNominal i)   = Just [i]
            typeIdentifiers (TyVar i)       = Just [i]
            typeIdentifiers TyStructural {} = Nothing
            typeIdentifiers (TyApp x y)     = concat <$> sequence [typeIdentifiers x, typeIdentifiers y]
            typeIdentifiers (TyFun a b) = do
                as <- typeIdentifiers a
                bs <- typeIdentifiers b
                pure (as <> bs)

            whenParsesToTypeDecl result assertions = do
                it "parses to a type decl" $
                    result `shouldSatisfy` is (_Right._AstDecl._DecDataType)
                when (is _Right result) assertions

            whenParsesToAbsTypeDecl result assertions = do
                it "parses to a type decl" $
                    result `shouldSatisfy` is (_Right._AstDecl._DecAbsType)
                when (is _Right result) assertions


        describe "abstract type" $ do
            result <- parseFile "Void.whippet"
            whenParsesToAbsTypeDecl result $
                it "has the expected type name" $
                    typeName result `shouldBe` [ident "Void"]

        describe "nullary constructor" $ do
            result <- parseFile "Unit.whippet"
            whenParsesToTypeDecl result $ do
                it "has the expected constructor" $
                    ctorLabels result `shouldBe` [ident "Unit"]
                it "has no parameters" $
                    ctorParamTypes result `shouldSatisfy` is _Empty

        describe "multiple nullary constructors" $ do
            result <- parseFile "Bool.whippet"
            whenParsesToTypeDecl result $ do
                it "has the expected constructors" $
                    ctorLabels result `shouldBe` [ident "True", ident "False"]
                it "has no parameters" $
                    ctorParamTypes result `shouldSatisfy` is _Empty

        describe "first constructor has a leading pipe" $ do
            result <- parseFile "CtorOptionalPipe.whippet"
            whenParsesToTypeDecl result $
                it "has the expected constructors" $
                    ctorLabels result `shouldBe` [ident "True", ident "False"]

        describe "single type parameter" $ do
            result <- parseFile "PhantomType.whippet"
            whenParsesToTypeDecl result $
                it "has the expected type parameter" $
                    typeParameters result `shouldBe` [ident "a"]

        describe "multiple type parameters" $ do
            result <- parseFile "CoerceType.whippet"
            whenParsesToTypeDecl result $
                it "has the expected type parameters" $
                    typeParameters result `shouldBe` [ident "source", ident "dest"]

        describe "constructor reference to type parameters" $ do
            result <- parseFile "Either.whippet"
            whenParsesToTypeDecl result $
                it "has the expected ctor parameter types" $
                    ctorParamTypes result `shouldBe` [ident "e", ident "a"]

    describe "parsing a function signature" $ do
        let fnType :: ParsedAst -> Text
            fnType ast =
                ast ^. _Right._AstDecl._DecFun._2.to typeToText

            fnName :: ParsedAst -> [Ident]
            fnName ast =
                ast ^.. _Right.identifier

            whenParsesToSigWithFn result assertions = do
                it "parses to a function signature" $
                    result `shouldSatisfy` is (_Right._AstDecl._DecFun)
                when (is _Right result) assertions

        describe "unary type signature" $ do
            result <- parseFile "UnitFunSig.whippet"
            whenParsesToSigWithFn result $ do
                it "has the expected identifier" $
                    fnName result `shouldBe` [ident "unit"]
                it "has the expected type parameters" $
                    fnType result `shouldBe` "Unit"

        describe "binary type signature" $ do
            result <- parseFile "IdentityFunSig.whippet"
            whenParsesToSigWithFn result $ do
                it "has the expected identifier" $
                    fnName result `shouldBe` [ident "identity"]
                it "has the expected type parameters" $
                    fnType result `shouldBe` "(a -> a)"

        describe "ternary type signature" $ do
            result <- parseFile "ConstFunSig.whippet"
            whenParsesToSigWithFn result $ do
                it "has the expected identifier" $
                    fnName result `shouldBe` [ident "const"]
                it "has the expected type parameters" $
                    fnType result `shouldBe` "(a -> (b -> a))"

        describe "type signature with paranthesised identifier" $ do
            result <- parseFile "FunctionTyParens.whippet"
            whenParsesToSigWithFn result $ do
                it "has the expected identifier" $
                    fnName result `shouldBe` [ident "const"]
                it "has the expected type parameters" $
                    fnType result `shouldBe` "(a -> (b -> a))"

        describe "type signature with type constructor parameter" $ do
            result <- parseFile "FunctionTyCtor.whippet"
            whenParsesToSigWithFn result $ do
                it "has the expected identifier" $
                    fnName result `shouldBe` [ident "getOpt"]
                it "has the expected type parameters" $
                    fnType result `shouldBe` "(a -> (Option a -> a))"

        describe "type signature with function type parameter" $ do
            result <- parseFile "ListMapFun.whippet"
            whenParsesToSigWithFn result $ do
                it "has the expected identifier" $
                    fnName result `shouldBe` [ident "map"]
                it "has the expected type parameters" $
                    fnType result `shouldBe` "((a -> b) -> (List a -> List b))"

        describe "type signature with structural type as input" $ do
            result <- parseFile "StructuralTypeParameterInput.whippet"
            whenParsesToSigWithFn result $ do
                it "has the expected identifier" $
                    fnName result `shouldBe` [ident "first"]
                it "has the expected type parameters" $
                    fnType result `shouldBe` "({fst: A, snd: B} -> A)"

        describe "type signature with structural type as output" $ do
            result <- parseFile "StructuralTypeParameterOutput.whippet"
            whenParsesToSigWithFn result $ do
                it "has the expected identifier" $
                    fnName result `shouldBe` [ident "box"]
                it "has the expected type parameters" $
                    fnType result `shouldBe` "(A -> {unpack: A})"


    -- * Expressions

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

            var = toListOf (_Right._EVar.identifier)

        describe "identifier ending with an underscore" $ do
            let result = parseExpr "x_"
            whenParsesToVar result $
                it "has the expected identifier" $
                    var result `shouldBe` [ident "x_"]

        describe "identifier starting with a question mark" $ do
            let result = parseExpr "?x"
            it "should fail to parse" $
                result `shouldSatisfy` is _Left

        describe "identifier containing a question mark" $ do
            let result = parseExpr "x?"
            whenParsesToVar result $
                it "has the expected identifier" $
                    var result `shouldBe` [ident "x?"]

        describe "identifier starting with a number" $ do
            let result = parseExpr "1x"
            it "should fail to parse" $
                pending
                -- result `shouldSatisfy` is _Left

        describe "identifier containing a number" $ do
            let result = parseExpr "x1"
            whenParsesToVar result $
                it "has the expected identifier" $
                    var result `shouldBe` [ident "x1"]


    describe "holes" $ do

        let whenParsesToHole result assertions = do
                it "parses to a hole" $
                    result `shouldSatisfy` is (_Right._EHole)
                when (is _Right result) assertions

            holeContent =
                 view (_Right._EHole.text)

        describe "underscore" $ do
            let result = parseExpr "_"
            whenParsesToHole result $
                it "has the expected identifier" $
                    holeContent result `shouldBe` "_"

        describe "named hole" $ do
            let result = parseExpr "_1"
            whenParsesToHole result $
                it "has the expected identifier" $
                    holeContent result `shouldBe` "_1"


    let itIsInt result =
            it "parses to an integer" $
                result `shouldSatisfy` is (_Right._ELit._LitInt)

    describe "integer literals" $ do

        describe "natural number" $ do
            let result = parseExpr "1"
            itIsInt result

        describe "signed positive integer" $ do
            let result = parseExpr "+1"
            itIsInt result

        describe "signed negative integer" $ do
            let result = parseExpr "-1"
            itIsInt result


    let itIsFloat result =
            it "parses to a float" $
                result `shouldSatisfy` is (_Right._ELit._LitScientific)

    describe "floating point literals" $ do

        describe "unsigned float" $ do
            let result = parseExpr "1.5"
            itIsFloat result

        describe "signed positive float" $ do
            let result = parseExpr "+1.5"
            itIsFloat result

        describe "signed negative float" $ do
            let result = parseExpr "-1.5"
            itIsFloat result

    describe "scientific literals" $ do

        describe "unsigned scientific" $ do
            let result = parseExpr "1e-6"
            itIsFloat result

        describe "positive scientific" $ do
            let result = parseExpr "+1e-6"
            itIsFloat result

        describe "negative scientific" $ do
            let result = parseExpr "-1e-6"
            itIsFloat result


    describe "string literal" $ do

        let whenParsesToString result assertions = do
                it "parses to a string" $
                    result `shouldSatisfy` is (_Right._ELit._LitString)
                when (is _Right result) assertions

            stringContent = view (_Right._ELit._LitString)

        describe "simple string literal" $ do
            result <- parseExprFromFile "SimpleStringLiteral.whippet"
            whenParsesToString result $
                it "has the expected content" $
                    stringContent result `shouldBe` "foo"

        describe "multiline string literal" $ do
            result <- parseExprFromFile "MultilineStringLiteral.whippet"
            whenParsesToString result $
                it "has the expected content" $
                    stringContent result `shouldBe` "foo\n bar"

        describe "string literal with escape sequence" $ do
            result <- parseExprFromFile "StringLiteralWithEscapes.whippet"
            whenParsesToString result $
                it "has the expected content" $
                    stringContent result `shouldBe` "foo\nbar"


    describe "character literal" $ do

        let whenParsesToChar result assertions = do
                it "parses to a char literal" $
                    result `shouldSatisfy` is (_Right._ELit._LitChar)
                when (is _Right result) assertions

            char :: Either Doc Expr -> [Char]
            char expr =
                expr ^.. _Right._ELit._LitChar

        describe "no character" $ do
            let result = parseExpr "''"
            it "fails to parse" $
                result `shouldSatisfy` is _Left

        describe "single character" $ do
            let result = parseExpr "'a'"
            whenParsesToChar result $
                it "has the expected content" $
                    char result `shouldBe` "a"


    describe "list literal" $ do

        let whenParsesToList result assertions = do
                it "parses to a list" $
                    result `shouldSatisfy` is (_Right._ELit._LitList)
                when (is _Right result) assertions

            listContent = view (_Right._ELit._LitList)

        describe "empty list literal" $ do
            let result = parseExpr "[]"
            whenParsesToList result $
                it "has the expected content" $
                    listContent result `shouldSatisfy` is _Empty

        describe "singleton list literal" $ do
            let result = parseExpr "[1]"
            whenParsesToList result $
                it "has the expected content" $
                    listContent result `shouldBe` [int 1]

        describe "comma-separated list entries" $ do
            let result = parseExpr "[1,2]"
            whenParsesToList result $
                it "has the expected content" $
                    listContent result `shouldBe` [int 1, int 2]

        describe "optional leading and trailing commas" $ do
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

        describe "empty record" $ do
            let result = parseExpr "{}"
            whenParsesToRecord result $
                it "has the expected content" $
                    recordContent result `shouldSatisfy` is _Empty

        describe "single field" $ do
            let result = parseExpr "{x:1}"
            whenParsesToRecord result $
                it "has the expected content" $
                    recordContent result `shouldBe` [(ident "x", int 1)]

        describe "comma-separated fields" $ do
            let result = parseExpr "{x:1,y:2}"
            whenParsesToRecord result $
                it "has the expected content" $
                    recordContent result `shouldBe` [ (ident "x", int 1)
                                                    , (ident "y", int 2)
                                                    ]

        describe "optional leading and trailing commas" $ do
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

        describe "simple annotation" $ do
            let result = parseExpr "1 : Int"
            whenParsesToAnnotation result $
                it "has the expected type" $
                    annType result `shouldBe` Just (nominalType "Int")

        describe "nested type annotations" $ do
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

        describe "simple expressions " $ do
            let result = parseExpr "if foo then bar else baz"
            whenParsesToIfThenElse result $
                it "has the expected values" $
                    subexpressions result `shouldBe` [var "foo", var "bar", var "baz"]


    let dVar :: Text -> Discriminator
        dVar x = DVar (Ident emptySpan x)

        dCtor :: Text -> Discriminator
        dCtor x = DCtor (Ident emptySpan x)

    describe "lambda" $ do

        let discriminators :: Either Doc Expr -> [Discriminator]
            discriminators expr =
                expr ^.. _Right._EFn.traverse.patDiscriminator

            bodyForms :: Either Doc Expr -> [Expr]
            bodyForms expr =
                expr ^.. _Right._EFn.traverse.patBody

            whenParsesToLambda result assertions = do
                it "parses to a lambda expression" $
                    result `shouldSatisfy` is (_Right._EFn)
                when (is _Right result) assertions


        describe "bare lambda" $ do

            describe "named binding" $ do
                let result = parseExpr "fn x -> 0"
                whenParsesToLambda result $ do
                    it "has the expected binder" $
                        discriminators result `shouldBe` [dVar "x"]
                    it "has the expected body" $
                        bodyForms result `shouldBe` [int 0]

            describe "named binding with type annotation" $ do
                let result = parseExpr "fn x: Int -> 0"
                whenParsesToLambda result $ do
                    it "has the expected binder" $
                        discriminators result `shouldBe` [dVar "x" `DAnn` nominalType "Int"]
                    it "has the expected body" $
                        bodyForms result `shouldBe` [int 0]

            describe "'as' pattern" $ do
                let result = parseExpr "fn u as Unit -> 1"
                whenParsesToLambda result $
                    it "has the expected binder" $
                        discriminators result `shouldBe` [dVar "u" `DAs` dCtor "Unit"]

            describe "structural type" $ do
                let result = parseExpr "fn {fst, snd} -> fst "
                whenParsesToLambda result $ do
                    it "has the expected binder" $
                        discriminators result `shouldBe` [DRec [dVar "fst", dVar "snd"]]
                    it "has the expected body" $
                        bodyForms result `shouldBe` [var "fst"]


        describe "branching lambda" $ do

            describe "single case" $ do
                let result = parseExpr "fn { x: Int -> 0 }"
                whenParsesToLambda result $ do
                    it "has the expected binder" $
                        discriminators result `shouldBe` [dVar "x" `DAnn` nominalType "Int"]
                    it "has the expected body" $
                        bodyForms result `shouldBe` [int 0]

            describe "optional leading pipe" $ do
                let result = parseExpr "fn { | x -> 0 }"
                whenParsesToLambda result $
                    it "parses a single case" $
                        length (bodyForms result) `shouldBe` 1

            describe "matching single nullary constructor" $ do
                let result = parseExpr "fn { Unit -> 0 }"
                whenParsesToLambda result $ do
                    it "has the expected binder" $
                        discriminators result `shouldBe` [dCtor "Unit"]
                    it "has the expected body" $
                        bodyForms result `shouldBe` [int 0]

            describe "matching multiple nullary constructors" $ do
                let result = parseExpr "fn { True -> 0 | False -> 1 }"
                whenParsesToLambda result $ do
                    it "has the expected binder" $
                        discriminators result `shouldBe` [dCtor "True", dCtor "False"]
                    it "has the expected body" $
                        bodyForms result `shouldBe` [int 0, int 1]

            describe "matching constructors with parameters" $ do
                let result = parseExpr "fn { None -> 0 | Some x -> 1 }"
                whenParsesToLambda result $
                    it "has the expected binders" $
                        discriminators result `shouldBe` [ dCtor "None"
                                                         , dCtor "Some" `DApp` dVar "x"
                                                         ]

            describe "matching constructor with multiple parameters" $ do
                let result = parseExpr "fn { Pair x y  -> 1 }"
                whenParsesToLambda result $
                    it "has the expected binder" $
                        discriminators result `shouldBe` [dCtor "Pair" `DApp` dVar "x" `DApp` dVar "y"]

            describe "'as' pattern" $ do
                let result = parseExpr "fn { u as Unit -> u }"
                whenParsesToLambda result $
                    it "has the expected binder" $
                        discriminators result `shouldBe` [dVar "u" `DAs` dCtor "Unit"]

            describe "matching record type" $ do
                let result = parseExpr "fn { {fst, snd} -> fst }"
                whenParsesToLambda result $ do
                    it "has the expected binder" $
                        discriminators result `shouldBe` [DRec [dVar "fst", dVar "snd"]]
                    it "has the expected body" $
                        bodyForms result `shouldBe` [var "fst"]

            describe "matching record type, leading comma" $ do
                let result = parseExpr "fn { {,fst} -> fst }"
                whenParsesToLambda result $
                    it "has the expected binder" $
                        discriminators result `shouldBe` [DRec [dVar "fst"]]

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

            dWildcard :: Text -> Discriminator
            dWildcard = DWildcard . ident

            whenParsesToMatch result assertions = do
                it "parses to a match expression" $
                    result `shouldSatisfy` is (_Right._EMatch)
                when (is _Right result) assertions

        describe "single case" $ do
            let result = parseExpr "match x with { y -> 0 }"
            whenParsesToMatch result $ do
                it "has the expected scrutinee" $
                    scrutinee result `shouldBe` [var "x"]
                it "has the expected binder" $
                    discriminators result `shouldBe` [dVar "y"]
                it "has the expected body" $
                    bodyForms result `shouldBe` [int 0]

        describe "matching multiple nullary constructors" $ do
            let result = parseExpr "match b with { True -> 0 | False -> 1 }"
            whenParsesToMatch result $ do
                it "has the expected scrutinee" $
                    scrutinee result `shouldBe` [var "b"]
                it "has the expected binder" $
                    discriminators result `shouldBe` [dCtor "True", dCtor "False"]
                it "has the expected body" $
                    bodyForms result `shouldBe` [int 0, int 1]

        describe "'as' pattern" $ do
            let result = parseExpr "match x with { u as Unit -> 1 }"
            whenParsesToMatch result $
                it "has the expected binder" $
                    discriminators result `shouldBe` [dVar "u" `DAs` dCtor "Unit"]

        describe "matching record type" $ do
            let result = parseExpr "match t with { {fst, snd} -> fst }"
            whenParsesToMatch result $ do
                it "has the expected scrutinee" $
                    scrutinee result `shouldBe` [var "t"]
                it "has the expected binder" $
                    discriminators result `shouldBe` [DRec [dVar "fst", dVar "snd"]]
                it "has the expected body" $
                    bodyForms result `shouldBe` [var "fst"]

        describe "matching record type, leading comma" $ do
            let result = parseExpr "match p with { {,fst} -> fst }"
            whenParsesToMatch result $ do
                it "has the expected scrutinee" $
                    scrutinee result `shouldBe` [var "p"]
                it "has the expected binder" $
                    discriminators result `shouldBe` [DRec [dVar "fst"]]

        describe "simple wildcard" $ do
            let result = parseExpr "match x with { _ -> 1 }"
            whenParsesToMatch result $
                it "has a wildcard binder" $
                    discriminators result `shouldBe` [dWildcard "_"]

        describe "named wildcard" $ do
            let result = parseExpr "match x with { _foo -> 1 }"
            whenParsesToMatch result $
                it "has a wildcard binder" $
                    discriminators result `shouldBe` [dWildcard "_foo"]

    describe "function application" $ do
        let whenParsesToApp result assertions = do
                it "parses to function application" $
                    result `shouldSatisfy` is (_Right._EApp)
                when (is _Right result) assertions

            function :: Either Doc Expr -> [Expr]
            function expr =
                expr ^.. _Right._EApp._1

            argument :: Either Doc Expr -> [Expr]
            argument expr =
                expr ^.. _Right._EApp._2

        describe "single argument" $ do
            let result = parseExpr "f x"
            whenParsesToApp result $ do
                it "has the expected expression" $
                    function result `shouldBe` [var "f"]
                it "has the expected argument" $
                    argument result `shouldBe` [var "x"]

        describe "curried application" $ do
            let result = parseExpr "f x y"
            whenParsesToApp result $ do
                it "has the expected expression" $
                    function result `shouldBe` [var "f" `EApp` var "x"]
                it "has the expected argument" $
                    argument result `shouldBe` [var "y"]

    describe "let...in" $ do

        let whenParsesToLet result assertions = do
                it "parses to a let expression" $
                    result `shouldSatisfy` is (_Right._ELet)
                when (is _Right result) assertions

            discriminator :: Either Doc Expr -> [Discriminator]
            discriminator expr =
                expr ^.. _Right._ELet._1

            definition :: Either Doc Expr -> [Expr]
            definition expr =
                expr ^.. _Right._ELet._2

            body :: Either Doc Expr -> [Expr]
            body expr =
                expr ^.. _Right._ELet._3

        describe "named binding" $ do
            let result = parseExpr "let x = 0 in 1"
            whenParsesToLet result $ do
                it "has the expected binder" $
                    discriminator result `shouldBe` [dVar "x"]
                it "has the expected definition" $
                    definition result `shouldBe` [int 0]
                it "has the expected body" $
                    body result `shouldBe` [int 1]

        describe "named binding with type annotation" $ do
            let result = parseExpr "let x: Int = 0 in 1"
            whenParsesToLet result $ do
                it "has the expected binder" $
                    discriminator result `shouldBe` [dVar "x" `DAnn` nominalType "Int"]
                it "has the expected definition" $
                    definition result `shouldBe` [int 0]
                it "has the expected body" $
                    body result `shouldBe` [int 1]

        describe "'as' pattern" $ do
            let result = parseExpr "let b as True = x in e"
            whenParsesToLet result $ do
                it "has the expected binder" $
                    discriminator result `shouldBe` [dVar "b" `DAs` dCtor "True"]
                it "has the expected definition" $
                    definition result `shouldBe` [var "x"]
                it "has the expected body" $
                    body result `shouldBe` [var "e"]

        describe "structural type" $ do
            let result = parseExpr "let {fst, snd} = p in snd"
            whenParsesToLet result $ do
                it "has the expected binder" $
                    discriminator result `shouldBe` [DRec [dVar "fst", dVar "snd"]]
                it "has the expected definition" $
                    definition result `shouldBe` [var "p"]
                it "has the expected body" $
                    body result `shouldBe` [var "snd"]

    describe "open statement" $ do

        let parseAst :: BS.ByteString -> ParsedAst
            parseAst =
                resultToEither . Trifecta.parseByteString (Parser.ast <* Trifecta.eof) mempty

            whenParsesToOpen result assertions = do
                it "parses to an 'open' statement" $
                    result `shouldSatisfy` is (_Right._AstOpen)
                when (is _Right result) assertions

            hidden :: ParsedAst -> [Ident]
            hidden ast =
                ast ^. _Right._AstOpen.openHiding._Just

            rename :: ParsedAst -> [Ident]
            rename ast =
                ast ^.. _Right._AstOpen.openAs._Just

            modId :: ParsedAst -> [Ident]
            modId ast =
                ast ^. _Right._AstOpen._Open._1._ModuleId.to NonEmpty.toList

        describe "simple open" $ do
            let result = parseAst "open M"
            whenParsesToOpen result $
                it "has the expected module ID" $
                    modId result `shouldBe` [ident "M"]

        describe "open module with path" $ do
            let result = parseAst "open M.N"
            whenParsesToOpen result $
                it "has the expected module ID" $
                    modId result `shouldBe` [ident "M", ident "N"]

        describe "open hiding" $ do
            let result = parseAst "open M hiding (foo, bar)"
            whenParsesToOpen result $ do
                it "has the expected module ID" $
                    modId result `shouldBe` [ident "M"]
                it "has the expected hidden identifiers" $
                    hidden result `shouldBe` [ident "foo", ident "bar"]

        describe "optional comma in 'hiding'" $ do
            let result = parseAst "open M hiding (,foo,bar)"
            whenParsesToOpen result $
                it "has the expected hidden identifiers" $
                    hidden result `shouldBe` [ident "foo", ident "bar"]

        describe "open with renaming" $ do
            let result = parseAst "open M as X"
            whenParsesToOpen result $ do
                it "has the expected module ID" $
                    modId result `shouldBe` [ident "M"]
                it "has the expected rebinding" $
                    rename result `shouldBe` [ident "X"]

        describe "open with renaming and hidden" $ do
            let result = parseAst "open M as X hiding (x,y)"
            whenParsesToOpen result $ do
                it "has the expected module ID" $
                    modId result `shouldBe` [ident "M"]
                it "has the expected rebinding" $
                    rename result `shouldBe` [ident "X"]
                it "has the expected hidden identifiers" $
                    hidden result `shouldBe` [ident "x", ident "y"]

    describe "open...in expression" $ do

        let whenParsesToOpen result assertions = do
                it "parses to an 'open' expression" $
                    result `shouldSatisfy` is (_Right._EOpen)
                when (is _Right result) assertions

            modId :: Either Doc Expr -> [Ident]
            modId ast =
                ast ^. _Right._EOpen._1._Open._1._ModuleId.to NonEmpty.toList

            hidden :: Either Doc Expr -> [Ident]
            hidden ast =
                ast ^. _Right._EOpen._1.openHiding._Just

            rename :: Either Doc Expr -> [Ident]
            rename ast =
                ast ^.. _Right._EOpen._1.openAs._Just

            body :: Either Doc Expr -> [Expr]
            body ast =
                ast ^.. _Right._EOpen._2

        describe "simple open" $ do
            let result = parseExpr "open M in 0"
            whenParsesToOpen result $ do
                it "has the expected module ID" $
                    modId result `shouldBe` [ident "M"]
                it "has the expected body" $
                    body result `shouldBe` [int 0]

        describe "open module with path" $ do
            let result = parseExpr "open M.N in 0"
            whenParsesToOpen result $ do
                it "has the expected module ID" $
                    modId result `shouldBe` [ident "M", ident "N"]
                it "has the expected body" $
                    body result `shouldBe` [int 0]

        describe "open hiding" $ do
            let result = parseExpr "open M hiding (foo, bar) in 0"
            whenParsesToOpen result $ do
                it "has the expected module ID" $
                    modId result `shouldBe` [ident "M"]
                it "has the expected hidden identifiers" $
                    hidden result `shouldBe` [ident "foo", ident "bar"]
                it "has the expected body" $
                    body result `shouldBe` [int 0]

        describe "open with renaming" $ do
            let result = parseExpr "open M as X in 0"
            whenParsesToOpen result $ do
                it "has the expected module ID" $
                    modId result `shouldBe` [ident "M"]
                it "has the expected rebinding" $
                    rename result `shouldBe` [ident "X"]
                it "has the expected body" $
                    body result `shouldBe` [int 0]

        describe "open with renaming and hidden" $ do
            let result = parseExpr "open M as X hiding (x,y) in 0"
            whenParsesToOpen result $ do
                it "has the expected identifier" $
                    modId result `shouldBe` [ident "M"]
                it "has the expected rebinding" $
                    rename result `shouldBe` [ident "X"]
                it "has the expected hidden identifiers" $
                    hidden result `shouldBe` [ident "x", ident "y"]
                it "has the expected body" $
                    body result `shouldBe` [int 0]
