module Endomorph.Lexer.Operator where

import Prelude hiding (and, div, min, mod, not, or)

import Text.Megaparsec (choice)
import Text.Megaparsec.Char (char, string)

import Endomorph.Lexer.Common (Parser)
import Endomorph.Token (Token(Operator), Operator(..))

operator :: Parser Token
operator = choice
  [ arrow
  , equality
  , greaterEquals
  , lessEquals
  , lShift
  , power
  , rShift
  , and
  , assign
  , divide
  , equality
  , greater
  , less
  , minus
  , modulo
  , not
  , or
  , plus
  , times ]

and :: Parser Token
and = do
  char '&'
  return $ Operator And

arrow :: Parser Token
arrow = do
  string "->"
  return $ Operator Arrow

assign :: Parser Token
assign = do
  char '='
  return $ Operator Assign

divide :: Parser Token
divide = do
  char '/'
  return $ Operator Divide

equality :: Parser Token
equality = do
  string "=="
  return $ Operator Equality

greater :: Parser Token
greater = do
  char '>'
  return $ Operator Greater

greaterEquals :: Parser Token
greaterEquals = do
  string ">="
  return $ Operator GreaterEquals

less :: Parser Token
less = do
  char '<'
  return $ Operator Less

lessEquals :: Parser Token
lessEquals = do
  string "<="
  return $ Operator LessEquals

lShift :: Parser Token
lShift = do
  string "<<"
  return $ Operator LShift

minus :: Parser Token
minus = do
  char '-'
  return $ Operator Minus

modulo :: Parser Token
modulo = do
  char '%'
  return $ Operator Modulo

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

power :: Parser Token
power = do
  string "**"
  return $ Operator Power

rShift :: Parser Token
rShift = do
  string ">>"
  return $ Operator RShift

times :: Parser Token
times = do
  char '*'
  return $ Operator Times

xor :: Parser Token
xor = do
  char '^'
  return $ Operator Xor
