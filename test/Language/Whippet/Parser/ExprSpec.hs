{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Parser.ExprSpec where

import           Control.Lens
import           Control.Lens.Extras
import           Control.Monad                      (when)
import qualified Data.ByteString.Internal           as BS
import qualified Data.List.NonEmpty                 as NonEmpty
import           Data.Text                          (Text)
import qualified Language.Whippet.Parser            as Parser
import           Language.Whippet.Parser.Lenses
import           Language.Whippet.Parser.ParseUtils
import           Language.Whippet.Parser.Types
import           Language.Whippet.PPrint
import           Test.Hspec
import           Text.PrettyPrint.ANSI.Leijen       (Doc)
import qualified Text.Trifecta                      as Trifecta

main :: IO ()
main = hspec spec

parseExpr :: BS.ByteString -> Either Doc Expr
parseExpr = parseString (Parser.expr <* Trifecta.eof)

parseExprFromFile file = runIO $
    parseFileFromResources (Parser.expr <* Trifecta.eof) file


spec :: Spec
spec = do

    describe "variable references" $ do

        let whenParsesToVar result assertions = do
                it "parses to a variable reference" $
                  result `shouldSatisfy` is (_Right._EVar)
                when (is _Right result) assertions

            var = toListOf (_Right._EVar.pprint')

        describe "identifier ending with an underscore" $ do
            let result = parseExpr "x_"
            whenParsesToVar result $
                it "has the expected identifier" $
                    var result `shouldBe` ["x_"]

        describe "identifier starting with a question mark" $ do
            let result = parseExpr "?x"
            it "should fail to parse" $
                result `shouldSatisfy` is _Left

        describe "identifier containing a question mark" $ do
            let result = parseExpr "x?"
            whenParsesToVar result $
                it "has the expected identifier" $
                    var result `shouldBe` ["x?"]

        describe "identifier starting with a number" $ do
            let result = parseExpr "1a"
            it "should fail to parse" $
                result `shouldSatisfy` is _Left

        describe "identifier containing a number" $ do
            let result = parseExpr "x1"
            whenParsesToVar result $
                it "has the expected identifier" $
                    var result `shouldBe` ["x1"]


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

            char :: Either Doc Expr -> String
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

        describe "simple annotation" $ do
            let result = parseExpr "1 : Int"
            whenParsesToAnnotation result $
                it "has the expected type" $ do
                    let t = result ^.. _Right._EAnnotation.annotationType
                    t `shouldBe` [nominalType "Int"]

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
                expr ^.. _Right._EMatch.matchExpr

            discriminators :: Either Doc Expr -> [Discriminator]
            discriminators expr =
                expr ^.. _Right._EMatch.matchPatterns.traverse.patDiscriminator

            guards :: Either Doc Expr -> [Guard]
            guards expr =
                expr ^.. _Right._EMatch.matchPatterns.traverse.patGuard._Just

            bodyForms :: Either Doc Expr -> [Expr]
            bodyForms expr =
                expr ^.. _Right._EMatch.matchPatterns.traverse.patBody

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

        describe "guards" $ do

            describe "simple 'if' guard" $ do
                let result = parseExpr "match x with { u if p -> 1 }"
                whenParsesToMatch result $ do
                    it "has the expected binder" $
                        discriminators result `shouldBe` [dVar "u"]
                    it "has the expected guard" $
                        guards result `shouldBe` [IfGuard (var "p")]

            describe "simple 'unless' guard" $ do
                let result = parseExpr "match x with { u unless p -> 1 }"
                whenParsesToMatch result $ do
                    it "has the expected binder" $
                        discriminators result `shouldBe` [dVar "u"]
                    it "has the expected guard" $
                        guards result `shouldBe` [UnlessGuard (var "p")]

            describe "guard with function application" $ do
                let result = parseExpr "match x with { u if p u -> 1 }"
                whenParsesToMatch result $ do
                    it "has the expected binder" $
                        discriminators result `shouldBe` [dVar "u"]
                    it "has the expected guard" $
                        guards result `shouldBe` [IfGuard (var "p" `eapp` var "u")]

            describe "guard with literals" $ do
                let result = parseExpr "match x with { u if p \"x\" -1 -> 1 }"
                whenParsesToMatch result $ do
                    it "has the expected binder" $
                        discriminators result `shouldBe` [dVar "u"]
                    it "has the expected guard" $
                        guards result `shouldBe` [IfGuard (var "p"
                                                           `eapp` str "x"
                                                           `eapp` int (-1))]


    describe "function application" $ do
        let whenParsesToApp result assertions = do
                it "parses to function application" $
                    result `shouldSatisfy` is (_Right._EApp)
                when (is _Right result) assertions

            function :: Either Doc Expr -> [Expr]
            function expr =
                expr ^.. _Right._EApp.appFn

            argument :: Either Doc Expr -> [Expr]
            argument expr =
                expr ^.. _Right._EApp.appArg

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
                    function result `shouldBe` [var "f" `eapp` var "x"]
                it "has the expected argument" $
                    argument result `shouldBe` [var "y"]

    describe "let bindings" $ do

        let whenParsesToLet result assertions = do
                it "parses to a let expression" $
                    result `shouldSatisfy` is (_Right._ELet)
                when (is _Right result) assertions

            discriminator :: Either Doc Expr -> [Discriminator]
            discriminator expr =
                expr ^.. _Right._ELet.letDiscriminator

            definition :: Either Doc Expr -> [Expr]
            definition expr =
                expr ^.. _Right._ELet.letValue

            body :: Either Doc Expr -> [Expr]
            body expr =
                expr ^.. _Right._ELet.letBody

        describe "named binding" $ do
            let result = parseExpr "let x = 0; 1"
            whenParsesToLet result $ do
                it "has the expected binder" $
                    discriminator result `shouldBe` [dVar "x"]
                it "has the expected definition" $
                    definition result `shouldBe` [int 0]
                it "has the expected body" $
                    body result `shouldBe` [int 1]

        describe "named binding with type annotation" $ do
            let result = parseExpr "let x: Int = 0; 1"
            whenParsesToLet result $ do
                it "has the expected binder" $
                    discriminator result `shouldBe` [dVar "x" `DAnn` nominalType "Int"]
                it "has the expected definition" $
                    definition result `shouldBe` [int 0]
                it "has the expected body" $
                    body result `shouldBe` [int 1]

        describe "'as' pattern" $ do
            let result = parseExpr "let b as True = x; e"
            whenParsesToLet result $ do
                it "has the expected binder" $
                    discriminator result `shouldBe` [dVar "b" `DAs` dCtor "True"]
                it "has the expected definition" $
                    definition result `shouldBe` [var "x"]
                it "has the expected body" $
                    body result `shouldBe` [var "e"]

        describe "structural type" $ do
            let result = parseExpr "let {fst, snd} = p; snd"
            whenParsesToLet result $ do
                it "has the expected binder" $
                    discriminator result `shouldBe` [DRec [dVar "fst", dVar "snd"]]
                it "has the expected definition" $
                    definition result `shouldBe` [var "p"]
                it "has the expected body" $
                    body result `shouldBe` [var "snd"]


    describe "open expression" $ do

        let whenParsesToOpen result assertions = do
                it "parses to an 'open' expression" $
                    result `shouldSatisfy` is (_Right._EOpen)
                when (is _Right result) assertions

            modId :: Either Doc Expr -> [Ident]
            modId ast =
                ast ^. _Right._EOpen._1._Open._1._QualId.to NonEmpty.toList

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
            let result = parseExpr "open M\n0"
            whenParsesToOpen result $ do
                it "has the expected module ID" $
                    modId result `shouldBe` [ident "M"]
                it "has the expected body" $
                    body result `shouldBe` [int 0]

        describe "open module with path" $ do
            let result = parseExpr "open M.N\n0"
            whenParsesToOpen result $ do
                it "has the expected module ID" $
                    modId result `shouldBe` [ident "M", ident "N"]
                it "has the expected body" $
                    body result `shouldBe` [int 0]

        describe "open hiding" $ do
            let result = parseExpr "open M hiding (foo, bar)\n0"
            whenParsesToOpen result $ do
                it "has the expected module ID" $
                    modId result `shouldBe` [ident "M"]
                it "has the expected hidden identifiers" $
                    hidden result `shouldBe` [ident "foo", ident "bar"]
                it "has the expected body" $
                    body result `shouldBe` [int 0]

        describe "open with renaming" $ do
            let result = parseExpr "open M as X\n0"
            whenParsesToOpen result $ do
                it "has the expected module ID" $
                    modId result `shouldBe` [ident "M"]
                it "has the expected rebinding" $
                    rename result `shouldBe` [ident "X"]
                it "has the expected body" $
                    body result `shouldBe` [int 0]

        describe "open with renaming and hidden" $ do
            let result = parseExpr "open M as X hiding (x,y)\n0"
            whenParsesToOpen result $ do
                it "has the expected identifier" $
                    modId result `shouldBe` [ident "M"]
                it "has the expected rebinding" $
                    rename result `shouldBe` [ident "X"]
                it "has the expected hidden identifiers" $
                    hidden result `shouldBe` [ident "x", ident "y"]
                it "has the expected body" $
                    body result `shouldBe` [int 0]
