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
  | AndAssign
  | Arrow
  | Assign
  | Divide
  | DivideAssign
  | Dot
  | Equality
  | Greater
  | GreaterEquals
  | Less
  | LessEquals
  | LShift
  | LShiftAssign
  | Minus
  | MinusAssign
  | Modulo
  | ModuloAssign
  | Not
  | NotEquals
  | Or
  | OrAssign
  | Plus
  | PlusAssign
  | Power
  | PowerAssign
  | Question
  | RShift
  | RShiftAssign
  | Times
  | TimesAssign
  | Xor
  | XorAssign
  deriving Show

data Punctuation
  = Colon
  | Comma
  | LBrace
  | LBracket
  | LParens
  | RBrace
  | RBracket
  | RParens
  | Semicolon
  deriving Show
