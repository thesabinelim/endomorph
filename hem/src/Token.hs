{-# LANGUAGE DuplicateRecordFields #-}

module Token where

data Token
  = EndOfInput
  | Identifier String
  | Literal Literal
  | Operator Operator
  deriving Show

data Literal
  = Boolean Boolean
  | Char
  | Integer IntegerLiteral
  | Float
  | String
  deriving Show

data Boolean = True | False deriving Show

data IntegerLiteral = IntegerLiteral
  { base :: Base
  , value :: String }
  deriving Show

data Base
  = Binary
  | Decimal
  | Hexadecimal
  | Octal
  deriving Show

data Operator
  = And
  | Div
  | Eq
  | EqEq
  | Gt
  | GtEq
  | Lshift
  | Lt
  | LtEq
  | Min
  | Mod
  | Not
  | Or
  | Xor
  | Plus
  | Pow
  | Rshift
  | Mult
  deriving Show
