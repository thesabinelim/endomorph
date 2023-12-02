module Endomorph.Lexer.Operator where

import Prelude hiding (and, div, min, mod, not, or)

import Text.Megaparsec (choice)
import Text.Megaparsec.Char (char, string)

import Endomorph.Lexer.Common (Parser)
import Endomorph.Token (Token(Operator), Operator(..))

operator :: Parser Token
operator = choice
  [ lShiftAssign
  , powerAssign
  , rShiftAssign
  , andAssign
  , arrow
  , divideAssign
  , equality
  , greaterEquals
  , lessEquals
  , lShift
  , minusAssign
  , moduloAssign
  , notEquals
  , orAssign
  , plusAssign
  , power
  , rShift
  , timesAssign
  , xorAssign
  , and
  , assign
  , divide
  , dot
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

andAssign :: Parser Token
andAssign = do
  string "&="
  return $ Operator AndAssign

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

divideAssign :: Parser Token
divideAssign = do
  char '/'
  return $ Operator DivideAssign

dot :: Parser Token
dot = do
  char '.'
  return $ Operator Dot

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

lShiftAssign :: Parser Token
lShiftAssign = do
  string "<<="
  return $ Operator LShiftAssign

minus :: Parser Token
minus = do
  char '-'
  return $ Operator Minus

minusAssign :: Parser Token
minusAssign = do
  string "-="
  return $ Operator MinusAssign

modulo :: Parser Token
modulo = do
  char '%'
  return $ Operator Modulo

moduloAssign :: Parser Token
moduloAssign = do
  string "%="
  return $ Operator ModuloAssign

not :: Parser Token
not = do
  char '!'
  return $ Operator Not

notEquals :: Parser Token
notEquals = do
  string "!="
  return $ Operator NotEquals

or :: Parser Token
or = do
  char '|'
  return $ Operator Or

orAssign :: Parser Token
orAssign = do
  string "|="
  return $ Operator OrAssign

plus :: Parser Token
plus = do
  char '+'
  return $ Operator Plus

plusAssign :: Parser Token
plusAssign = do
  string "+="
  return $ Operator PlusAssign

power :: Parser Token
power = do
  string "**"
  return $ Operator Power

powerAssign :: Parser Token
powerAssign = do
  string "**="
  return $ Operator PowerAssign

question :: Parser Token
question = do
  char '?'
  return $ Operator Question

rShift :: Parser Token
rShift = do
  string ">>"
  return $ Operator RShift

rShiftAssign :: Parser Token
rShiftAssign = do
  string ">>="
  return $ Operator RShiftAssign

times :: Parser Token
times = do
  char '*'
  return $ Operator Times

timesAssign :: Parser Token
timesAssign = do
  string "*="
  return $ Operator TimesAssign

xor :: Parser Token
xor = do
  char '^'
  return $ Operator Xor

xorAssign :: Parser Token
xorAssign = do
  string "^="
  return $ Operator XorAssign
