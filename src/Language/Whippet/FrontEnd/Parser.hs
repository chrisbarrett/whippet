{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
module Language.Whippet.Frontend.Parser where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Trans           (MonadIO)
import           Data.Monoid
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Language.Whippet.Frontend.AST
import           Text.Parser.Token.Style
import           Text.Trifecta                 hiding (ident)
import qualified Text.Trifecta                 as Trifecta

-- * Parser definitions

parseFile :: MonadIO m => FilePath -> m (Result (AST Span))
parseFile = parseFromFileEx ast

ast :: Parser (AST Span)
ast = whiteSpace *> (module' <|> signature <|> astDecl)

signature :: Parser (AST Span)
signature = do
    reserved "signature"
    AstSignature <$> ident <*> braces (many decl)

module' :: Parser (AST Span)
module' = do
    reserved "module"
    AstModule <$> ident <*> braces (many ast)

astDecl :: Parser (AST Span)
astDecl = AstDecl <$> decl

typeDecl :: Parser (Decl Span)
typeDecl = do
    s <- position
    l <- line
    let pos = (s, l)

    reserved "type"
    id <- ident
    tyArgs <- many typeParameter

    eq <- optional equals
    case eq of
      Just _  -> concreteType pos id tyArgs
      Nothing -> abstractType pos id tyArgs

  where
    concreteType (start, ln) id tyArgs = do
        cs <- optional pipe *> constructor `sepBy1` pipe
        end <- position
        let span = Span start end ln
        pure (DecDataType span id tyArgs cs)

    abstractType (start, ln) id tyArgs = do
        end <- position
        let span = Span start end ln
        pure (DecAbsType span id tyArgs)


constructor :: Parser (Ctor Span)
constructor = do
    ((id, ps) :~ span) <- spanned parser
    pure (Ctor span id ps)
  where
    parser = do
        id <- ident
        ps <- many typeRef
        pure (id, ps)


field :: Parser (Field Span)
field = do
    let parser = (,) <$> (ident <?> "field name")
                     <*> (colon *> typeRef)
    ((id, ty) :~ span) <- spanned parser
    pure (Field span id ty)


decl :: Parser (Decl Span)
decl = fnDecl <|> recordDecl <|> typeDecl

fnDecl :: Parser (Decl Span)
fnDecl = do
    s <- position
    ln <- line
    let mkSpan end = Span s end ln

    reserved "let"
    id <- ident
    colon
    parameters <- typeRef `sepBy1` arrow
    span <- mkSpan <$> position
    pure (DecFun span id parameters)
    <?> "let declaration"

arrow :: Parser ()
arrow = do
    token (string "->")
    pure ()

recordDecl :: Parser (Decl Span)
recordDecl = do
    s <- position
    ln <- line
    let mkSpan end = Span s end ln

    reserved "record"
    id <- ident
    tyArgs <- many typeParameter
    equals
    flds <- recordFields
    s <- mkSpan <$> position
    pure (DecRecordType s id tyArgs flds)


typeRef :: Parser (Type Span)
typeRef = try (parens parser) <|> parser
  where
    parser = do
        start <- position
        ln <- line
        let mkSpan = \end -> Span start end ln
        structuralType mkSpan <|> functionType mkSpan <|> nominalType mkSpan

    functionType mkSpan = do
        ps <- parens (typeRef `sepBy1` arrow)
        s <- mkSpan <$> position
        pure (TyFun s ps)

    nominalType mkSpan = do
        i <- ident
        ps <- many ident
        s <- mkSpan <$> position
        pure (TyNominal s i ps)
        <?> "type name"

    structuralType mkSpan = do
        flds <- recordFields
        end <- position
        pure (TyStructural (mkSpan end) flds)

recordFields :: Parser [Field Span]
recordFields = braces (optional comma *> field `sepBy1` comma)

-- * Token types

typeParameter :: Parser (TypeParameter Span)
typeParameter =
    TypeParameter <$> ident
      <?> "type parameter"

-- * Helpers

style :: IdentifierStyle Parser
style = emptyIdents
        & styleReserved .~ reservedWords
        & styleStart .~ (letter <|> char '_')
        & styleLetter .~ (alphaNum <|> oneOf "_?")
  where
    reservedWords = ["module", "signature", "type", "record", "let"]

ident :: Parser (Ident Span)
ident = do
    (s :~ span) <- spanned (Trifecta.ident style)
    pure (Ident span s)

reserved :: String -> Parser ()
reserved = reserve style

pipe :: Parser ()
pipe = reserved "|"

equals :: Parser ()
equals = reserved "="
