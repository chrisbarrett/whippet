module Language.Whippet.Frontend.ASTHelpers where

import           Control.Lens
import           Data.Text                     (Text)
import           Language.Whippet.Frontend.AST

text :: Lens' (Ident s) Text
text = identLabel

-- * Identifier lenses

class HasIdentifier a where
    identifier :: Lens' (a s) (Ident s)

instance HasIdentifier Ident where
    identifier = id

instance HasIdentifier Type where
    identifier = typeIdent

instance HasIdentifier TypeParameter where
    identifier = typeParameterIdent

instance HasIdentifier Ctor where
    identifier = ctorIdent

instance HasIdentifier Field where
    identifier = fieldIdent

instance HasIdentifier AST where
    identifier =
        lens getIdent setIdent
      where
        getIdent :: AST s -> Ident s
        getIdent (AstModule _ i _)       = i
        getIdent (AstSignature _ i _)    = i
        getIdent (AstAbstractType _ i _) = i
        getIdent (AstDataType _ i _ _)   = i
        getIdent (AstRecordType _ i _ _) = i

        setIdent :: AST s -> Ident s -> AST s
        setIdent (AstModule p _ x)       i = AstModule p i x
        setIdent (AstSignature p _ x)    i = AstSignature p i x
        setIdent (AstAbstractType p _ x) i = AstAbstractType p i x
        setIdent (AstDataType p _ x y)   i = AstDataType p i x y
        setIdent (AstRecordType p _ x y) i = AstRecordType p i x y

-- * Source position lenses

class HasPos a where
    srcPos :: Lens' (a s) s

instance HasPos Ident where
    srcPos = identPos

instance HasPos Type where
    srcPos = typePos

instance HasPos TypeParameter where
    srcPos = _TypeParameter.srcPos
