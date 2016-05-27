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
import           Text.Trifecta
import qualified Text.Trifecta                 as Trifecta

-- * Parser definitions

parseFile :: MonadIO m => FilePath -> m (Result (AST Span))
parseFile = parseFromFileEx ast

ast :: Parser (AST Span)
ast = whiteSpace *> (module' <|> signature <|> astDecl)

signature :: Parser (AST Span)
signature = do
    reserved "signature"
    AstSignature <$> typeName <*> braces (many decl)

module' :: Parser (AST Span)
module' = do
    reserved "module"
    AstModule <$> typeName <*> braces (many ast)

astDecl :: Parser (AST Span)
astDecl = AstDecl <$> decl

typeDecl :: Parser (Decl Span)
typeDecl = do
    s <- position
    l <- line
    let pos = (s, l)

    reserved "type"
    ident <- typeName
    tyArgs <- many typeParameter

    eq <- optional equals
    case eq of
      Just _  -> concreteType pos ident tyArgs
      Nothing -> abstractType pos ident tyArgs

  where
    concreteType (start, ln) ident tyArgs = do
        cs <- optional pipe *> constructor `sepBy1` pipe
        end <- position
        let span = Span start end ln
        pure (DecDataType span ident tyArgs cs)

    abstractType (start, ln) ident tyArgs = do
        end <- position
        let span = Span start end ln
        pure (DecAbsType span ident tyArgs)


constructor :: Parser (Ctor Span)
constructor = do
    ((id, ps) :~ span) <- spanned parser
    pure (Ctor span id ps)
  where
    parser = do
        ident <- typeName
        ps <- many typeRef
        pure (ident, ps)


field :: Parser (Field Span)
field = do
    let parser = (,) <$> (identifier' <?> "field name")
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
    ident <- identifier'
    colon
    parameters <- typeRef `sepBy1` arrow
    span <- mkSpan <$> position
    pure (DecFn span ident parameters)
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
    ident <- typeName
    tyArgs <- many typeParameter
    equals
    flds <- recordFields
    s <- mkSpan <$> position
    pure (DecRecordType s ident tyArgs flds)


typeRef :: Parser (Type Span)
typeRef = do
    start <- position
    ln <- line
    let mkSpan = \end -> Span start end ln
    structuralType mkSpan <|> nominalType mkSpan
  where
    nominalType mkSpan = do
        i <- token ((:) <$> letter <*> many (alphaNum <|> oneOf "_"))
        s <- mkSpan <$> position
        pure (TyNominal s (Ident s (Text.pack i)))
        <?> "type name"

    structuralType mkSpan = do
        flds <- recordFields
        end <- position
        pure (TyStructural (mkSpan end) flds)

recordFields :: Parser [Field Span]
recordFields = braces (optional comma *> field `sepBy1` comma)

-- * Token types

typeParameter :: Parser (TypeParameter Span)
typeParameter = do
    TypeParameter <$> tokenLike Ident ((:) <$> lower <*> many (alphaNum <|> oneOf "_"))
      <?> "type parameter"

typeName :: Parser (Ident Span)
typeName =
    tokenLike Ident ((:) <$> upper <*> many (alphaNum <|> oneOf "_"))
      <?> "type name"

identifier' :: Parser (Ident Span)
identifier' = do
    tokenLike Ident ((:) <$> lower <*> many (alphaNum <|> oneOf "_-?"))
      <?> "identifier"

tokenLike :: (Span -> Text -> t) -> Parser String -> Parser t
tokenLike f p = do
    (id :~ span) <- spanned (token (Text.pack <$> p))
    pure (f span id)


-- * Helpers

style :: IdentifierStyle Parser
style = emptyIdents {_styleReserved = reservedChars}
  where
    reservedChars = ["module", "signature", "type", "record", "let"]

reserved :: String -> Parser ()
reserved = reserve style

pipe :: Parser ()
pipe = reserved "|"

equals :: Parser ()
equals = reserved "="
