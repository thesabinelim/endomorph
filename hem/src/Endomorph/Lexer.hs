module Endomorph.Lexer where

import Data.Char (isSpace)
import Data.Void (Void)
import Text.Megaparsec
  ( between
  , choice
  , eof
  , many
  , Parsec
  , ParseErrorBundle
  , runParser
  , takeWhile1P )
import Text.Megaparsec.Char (space1)

import Endomorph.Lexer.Common (Parser)
import Endomorph.Lexer.Identifier (identifier)
import Endomorph.Lexer.Literal (literal)
import Endomorph.Lexer.Operator (operator)
import Endomorph.Lexer.Punctuation (punctuation)
import Endomorph.Token (Token(EndOfInput, Whitespace))

lex :: String -> Either (ParseErrorBundle String Void) [Token]
lex = runParser (tokens <* endOfInput) "stdin"

tokens :: Parser [Token]
tokens = many token

token :: Parser Token
token = choice
  [ identifier
  , literal
  , operator
  , punctuation
  , whitespace ]

endOfInput :: Parser Token
endOfInput = do
  eof
  return EndOfInput

whitespace :: Parser Token
whitespace = do
  spaces <- takeWhile1P (Just "whitespace") isSpace
  return $ Whitespace spaces
