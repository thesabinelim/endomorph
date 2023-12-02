module Endomorph.Lexer.Punctuation where

import Text.Megaparsec (choice)
import Text.Megaparsec.Char (char, string)

import Endomorph.Lexer.Common (Parser)
import Endomorph.Lexer.Util (symbol)
import Endomorph.Token (Token(Punctuation), Punctuation(..))

punctuation :: Parser Token
punctuation = choice
  [ colon
  , comma
  , lBrace
  , lBracket
  , lParens
  , rBrace
  , rBracket
  , rParens
  , semicolon ]

colon :: Parser Token
colon = symbol ":" $ Punctuation Colon

comma :: Parser Token
comma = symbol "," $ Punctuation Comma

lBrace :: Parser Token
lBrace = symbol "{" $ Punctuation LBrace

lBracket :: Parser Token
lBracket = symbol "[" $ Punctuation LBracket

lParens :: Parser Token
lParens = symbol "(" $ Punctuation LParens

rBrace :: Parser Token
rBrace = symbol "}" $ Punctuation RBrace

rParens :: Parser Token
rParens = symbol ")" $ Punctuation RParens

rBracket :: Parser Token
rBracket = symbol "]" $ Punctuation RBracket

semicolon :: Parser Token
semicolon = symbol ";" $ Punctuation Semicolon
