{-# LANGUAGE OverloadedStrings #-}
module Language.Whippet.Frontend.Parser.TypesSpec where

import           Control.Lens
import           Control.Lens.Extras
import           Control.Monad                               (when)
import           Data.Monoid                                 ((<>))
import           Data.Text                                   (Text)
import           Debug.Trace
import           Language.Whippet.Frontend.AST
import qualified Language.Whippet.Frontend.Parser            as Parser
import           Language.Whippet.Frontend.Parser.ParseUtils
import           Language.Whippet.Frontend.PPrint
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "parsing an abstract type" $ do
        result <- parseFile "Void.whippet"

        it "parses to a type decl" $
            result `shouldSatisfy` is (_Right._AstDecl._DecAbsType)

        when (is _Right result) $
            it "has the expected type name" $ do
                let typeName = result ^. _Right ._AstDecl ._DecAbsType .absTypeIdent .pprint'
                typeName `shouldBe` "Void"


    describe "parsing a record declaration" $ do
        let fieldLabels :: ParsedAst -> [Text]
            fieldLabels ast =
                ast ^.. _Right
                      ._AstDecl
                      ._DecRecordType
                      .recordTypeFields
                      .traverse
                      .fieldIdent
                      .pprint'

            fieldTypes :: ParsedAst -> [Text]
            fieldTypes ast =
                ast ^.. _Right
                      ._AstDecl
                      ._DecRecordType
                      .recordTypeFields
                      .traverse
                      .fieldType
                      .pprint'

            identifier :: ParsedAst -> Text
            identifier ast =
                ast ^. _Right
                     ._AstDecl
                     ._DecRecordType
                     .recordTypeIdent
                     .pprint'

            whenParsesToRecordDecl result assertions = do
                it "parses to a record decl" $
                    result `shouldSatisfy` is (_Right._AstDecl._DecRecordType)
                when (is _Right result) assertions

        describe "record type" $ do
            result <- parseFile "IntPair.whippet"
            whenParsesToRecordDecl result $ do
                it "has the expected identifier" $
                    identifier result `shouldBe` "IntPair"
                it "has the expected fields" $
                    fieldLabels result `shouldBe` ["fst", "snd"]
                it "has the expected field types" $
                    fieldTypes result `shouldBe` ["Int", "Int"]

        describe "record type with type parameters" $ do
            result <- parseFile "Pair.whippet"
            whenParsesToRecordDecl result $ do
                it "has the expected fields" $
                    fieldLabels result `shouldBe` ["fst", "snd"]
                it "has the expected field types" $
                    fieldTypes result `shouldBe` ["a", "b"]

        describe "record type with comma before first field" $ do
            result <- parseFile "RecordOptionalLeadingComma.whippet"
            whenParsesToRecordDecl result $
                it "has the expected fields" $
                    fieldLabels result `shouldBe` ["fst", "snd"]

    describe "parsing a type declaration" $ do
        let ctorsFromAst :: ParsedAst -> [Ctor]
            ctorsFromAst ast =
                ast ^. _Right._AstDecl._DecDataType.dataTypeCtors

            typeName :: ParsedAst -> Text
            typeName ast =
                ast ^. _Right._AstDecl._DecDataType.dataTypeIdent.pprint'

            typeParameters :: ParsedAst -> [Text]
            typeParameters ast =
                ast ^.. _Right._AstDecl._DecDataType.dataTypeParams.traverse.pprint'

            ctorLabels :: ParsedAst -> [Text]
            ctorLabels ast =
                ast ^.. to ctorsFromAst.traverse.ctorIdent.pprint'

            ctorParamTypes :: ParsedAst -> [Text]
            ctorParamTypes ast =
                ast ^.. to ctorsFromAst.traverse.ctorParams.traverse
                       .to typeIdentifiers._Just.each

            typeIdentifiers :: Type -> Maybe [Text]
            typeIdentifiers (TyVar i)       = Just [pprint i]
            typeIdentifiers TyStructural {} = Nothing
            typeIdentifiers (TyApp x y)     = concat <$> sequence [typeIdentifiers x, typeIdentifiers y]

            typeIdentifiers (TyNominal t) =
                Just [pprint t]

            typeIdentifiers (TyArrow a b) = do
                as <- typeIdentifiers a
                bs <- typeIdentifiers b
                pure (as <> bs)

            whenParsesToTypeDecl result assertions = do
                it "parses to a type decl" $
                    result `shouldSatisfy` is (_Right._AstDecl._DecDataType)
                when (is _Right result) assertions

        describe "nullary constructor" $ do
            result <- parseFile "Unit.whippet"
            whenParsesToTypeDecl result $ do
                it "has the expected constructor" $
                    ctorLabels result `shouldBe` ["Unit"]
                it "has no parameters" $
                    ctorParamTypes result `shouldSatisfy` is _Empty

        describe "multiple nullary constructors" $ do
            result <- parseFile "Bool.whippet"
            whenParsesToTypeDecl result $ do
                it "has the expected constructors" $
                    ctorLabels result `shouldBe` ["True", "False"]
                it "has no parameters" $
                    ctorParamTypes result `shouldSatisfy` is _Empty

        describe "first constructor has a leading pipe" $ do
            result <- parseFile "CtorOptionalPipe.whippet"
            whenParsesToTypeDecl result $
                it "has the expected constructors" $
                    ctorLabels result `shouldBe` ["True", "False"]

        describe "single type parameter" $ do
            result <- parseFile "PhantomType.whippet"
            whenParsesToTypeDecl result $
                it "has the expected type parameter" $
                    typeParameters result `shouldBe` ["a"]

        describe "multiple type parameters" $ do
            result <- parseFile "CoerceType.whippet"
            whenParsesToTypeDecl result $
                it "has the expected type parameters" $
                    typeParameters result `shouldBe` ["source", "dest"]

        describe "constructor reference to type parameters" $ do
            result <- parseFile "Either.whippet"
            whenParsesToTypeDecl result $
                it "has the expected ctor parameter types" $
                    ctorParamTypes result `shouldBe` ["e", "a"]
