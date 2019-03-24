{-# OPTIONS_GHC -fno-warn-name-shadowing -XFlexibleContexts #-}

module Lexer where

import Control.Monad.Identity
import AST
import Text.Parsec.Prim
import Text.Parsec.Char
import qualified Text.Parsec.Token as T

langDef :: T.GenLanguageDef String Int Identity
langDef = T.LanguageDef {
              T.caseSensitive = False
            , T.commentStart = "{-"
            , T.commentEnd = "-}"
            , T.commentLine = "--"
            , T.nestedComments = False
            , T.identStart = letter
            , T.identLetter   = alphaNum <|> oneOf "_'"
            , T.opStart       = T.opLetter langDef
            , T.opLetter      = oneOf ":*+<=-"
            , T.reservedNames = ["WHILE", "DO",  "IF", "THEN", "ELSE"
                                , "SKIP", "TRUE", "FALSE"]
            , T.reservedOpNames  = [":=", "+", "*", "-", "<=", "||", "&&"]}

lexer :: T.GenTokenParser String Int Identity
lexer = T.makeTokenParser langDef

reserved       = T.reserved lexer
reservedOp     = T.reservedOp lexer
charLiteral    = T.charLiteral lexer
stringLiteral  = T.stringLiteral lexer
natural        = T.natural lexer
symbol         = T.symbol lexer
lexeme         = T.lexeme lexer
whiteSpace     = T.whiteSpace lexer
semi           = T.semi lexer
parens         = T.parens lexer
semiSep        = T.semiSep lexer
sepiSep1       = T.semiSep1 lexer
commaSep       = T.commaSep lexer
commaSep1      = T.commaSep1 lexer

identifier :: (Char -> Bool) -> ParseM Id
identifier pred =
    try (do (c:cs) <- T.identifier lexer
            guard (pred c)
            return (c:cs))


type ParseM = Parsec String Int
