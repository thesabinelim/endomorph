module Endomorph.Token where

data Token
  = EndOfInput
  | Identifier String
  | Keyword Keyword
  | Literal Literal
  | Operator Operator
  | Punctuation Punctuation
  deriving Show

data Keyword
  = Else
  | Export
  | For
  | If
  | Import
  | Let
  | Mut
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
  | Arrow
  | Assign
  | Divide
  | Equality
  | Greater
  | GreaterEquals
  | Less
  | LessEquals
  | LShift
  | Minus
  | Modulo
  | Not
  | Or
  | Plus
  | Power
  | RShift
  | Times
  | Xor
  deriving Show

data Punctuation
  = LBrace
  | LBracket
  | LParens
  | RBrace
  | RBracket
  | RParens
  deriving Show
