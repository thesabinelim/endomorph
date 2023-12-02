module Endomorph.Lexer.Operator where

import Prelude hiding (and, div, min, mod, not, or)

import Text.Megaparsec (choice)
import Text.Megaparsec.Char (char, string)

import Endomorph.Lexer.Common (Parser)
import Endomorph.Lexer.Util (symbol)
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
and = symbol "&" $ Operator And

andAssign :: Parser Token
andAssign = symbol "&=" $ Operator AndAssign

arrow :: Parser Token
arrow = symbol "->" $ Operator Arrow

assign :: Parser Token
assign = symbol "=" $ Operator Assign

divide :: Parser Token
divide = symbol "/" $ Operator Divide

divideAssign :: Parser Token
divideAssign = symbol "/=" $ Operator DivideAssign

dot :: Parser Token
dot = symbol "." $ Operator Dot

equality :: Parser Token
equality = symbol "==" $ Operator Equality

greater :: Parser Token
greater = symbol ">" $ Operator Greater

greaterEquals :: Parser Token
greaterEquals = symbol ">=" $ Operator GreaterEquals

less :: Parser Token
less = symbol "<" $ Operator Less

lessEquals :: Parser Token
lessEquals = symbol "<=" $ Operator LessEquals

lShift :: Parser Token
lShift = symbol "<<" $ Operator LShift

lShiftAssign :: Parser Token
lShiftAssign = symbol "<<=" $ Operator LShiftAssign

minus :: Parser Token
minus = symbol "-" $ Operator Minus

minusAssign :: Parser Token
minusAssign = symbol "-=" $ Operator MinusAssign

modulo :: Parser Token
modulo = symbol "%" $ Operator Modulo

moduloAssign :: Parser Token
moduloAssign = symbol "%=" $ Operator ModuloAssign

not :: Parser Token
not = symbol "!" $ Operator Not

notEquals :: Parser Token
notEquals = symbol "!=" $ Operator NotEquals

or :: Parser Token
or = symbol "|" $ Operator Or

orAssign :: Parser Token
orAssign = symbol "|=" $ Operator OrAssign

plus :: Parser Token
plus = symbol "+" $ Operator Plus

plusAssign :: Parser Token
plusAssign = symbol "+=" $ Operator PlusAssign

power :: Parser Token
power = symbol "**" $ Operator Power

powerAssign :: Parser Token
powerAssign = symbol "**=" $ Operator PowerAssign

question :: Parser Token
question = symbol "?" $ Operator Question

rShift :: Parser Token
rShift = symbol ">>" $ Operator RShift

rShiftAssign :: Parser Token
rShiftAssign = symbol ">>=" $ Operator RShiftAssign

times :: Parser Token
times = symbol "*" $ Operator Times

timesAssign :: Parser Token
timesAssign = symbol "*=" $ Operator TimesAssign

xor :: Parser Token
xor = symbol "^" $ Operator Xor

xorAssign :: Parser Token
xorAssign = symbol "^=" $ Operator XorAssign
