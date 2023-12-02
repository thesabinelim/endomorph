module Endomorph.Lexer.Operator where

import Prelude hiding (and, div, min, mod, not, or)

import Text.Megaparsec (choice)
import Text.Megaparsec.Char (char, string)

import Endomorph.Lexer.Common (Parser)
import Endomorph.Token (Token(Operator), Operator(..))

operator :: Parser Token
operator = choice
  [ eqEq
  , gtEq
  , lshift
  , ltEq
  , pow
  , rshift
  , and
  , div
  , eq
  , gt
  , lt
  , min
  , mod
  , mult
  , not
  , or
  , plus ]

and :: Parser Token
and = do
  char '&'
  return $ Operator And

div :: Parser Token
div = do
  char '/'
  return $ Operator Div

eqEq :: Parser Token
eqEq = do
  string "=="
  return $ Operator EqEq

eq :: Parser Token
eq = do
  char '='
  return $ Operator Eq

gt :: Parser Token
gt = do
  char '>'
  return $ Operator Gt

gtEq :: Parser Token
gtEq = do
  string ">="
  return $ Operator GtEq

lshift :: Parser Token
lshift = do
  string "<<"
  return $ Operator Lshift

lt :: Parser Token
lt = do
  char '<'
  return $ Operator Lt

ltEq :: Parser Token
ltEq = do
  string "<="
  return $ Operator LtEq

min :: Parser Token
min = do
  char '-'
  return $ Operator Min

mod :: Parser Token
mod = do
  char '%'
  return $ Operator Mod

mult :: Parser Token
mult = do
  char '*'
  return $ Operator Mult

not :: Parser Token
not = do
  char '!'
  return $ Operator Not

or :: Parser Token
or = do
  char '|'
  return $ Operator Or

plus :: Parser Token
plus = do
  char '+'
  return $ Operator Plus

pow :: Parser Token
pow = do
  string "**"
  return $ Operator Pow

rshift :: Parser Token
rshift = do
  string ">>"
  return $ Operator Rshift

xor :: Parser Token
xor = do
  char '^'
  return $ Operator Xor
