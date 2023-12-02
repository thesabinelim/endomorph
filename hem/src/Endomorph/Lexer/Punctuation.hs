module Endomorph.Lexer.Punctuation where

import Text.Megaparsec (choice)
import Text.Megaparsec.Char (char, string)

import Endomorph.Lexer.Common (Parser)
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
colon = do
  char ':'
  return $ Punctuation Colon

comma :: Parser Token
comma = do
  char ','
  return $ Punctuation Comma

lBrace :: Parser Token
lBrace = do
  char '{'
  return $ Punctuation LBrace

lBracket :: Parser Token
lBracket = do
  char '['
  return $ Punctuation LBracket

lParens :: Parser Token
lParens = do
  char '('
  return $ Punctuation LParens

rBrace :: Parser Token
rBrace = do
  char '}'
  return $ Punctuation RBrace

rParens :: Parser Token
rParens = do
  char ')'
  return $ Punctuation RParens

rBracket :: Parser Token
rBracket = do
  char ']'
  return $ Punctuation RBracket

semicolon :: Parser Token
semicolon = do
  char ';'
  return $ Punctuation Semicolon
