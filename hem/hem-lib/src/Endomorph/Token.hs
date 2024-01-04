module Endomorph.Token where

data Token
  = Comment String
  | EndOfInput
  | Identifier String
  | Keyword Keyword
  | Literal Literal
  | Operator Operator
  | Punctuator Punctuator
  | Whitespace String
  deriving (Eq, Show)

data Keyword
  = Else
  | Export
  | For
  | From
  | If
  | Import
  | Let
  | Mut
  deriving (Eq, Show)

data Literal
  = Boolean Boolean
  | Char
  | Integer IntegerLiteral
  | Float
  | String
  deriving (Eq, Show)

data Boolean = True | False deriving (Eq, Show)

data IntegerLiteral = IntegerLiteral
  { base :: Base,
    value :: String
  }
  deriving (Eq, Show)

data Base
  = Binary
  | Decimal
  | Hexadecimal
  | Octal
  deriving (Eq, Show)

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
  | Spread
  | Times
  | TimesAssign
  | Xor
  | XorAssign
  deriving (Eq, Show)

data Punctuator
  = Colon
  | Comma
  | LBrace
  | LBracket
  | LParens
  | RBrace
  | RBracket
  | RParens
  deriving (Eq, Show)
