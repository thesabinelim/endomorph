module Endomorph.Lexer where

import Data.Void (Void)
import Text.Megaparsec
  ( between
  , choice
  , eof
  , many
  , Parsec
  , ParseErrorBundle
  , runParser )
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L
  ( lexeme
  , skipLineComment
  , skipBlockComment
  , space
  , symbol )

import Endomorph.Lexer.Common (Parser)
import Endomorph.Lexer.Identifier (identifier)
import Endomorph.Lexer.Literal (literal)
import Endomorph.Lexer.Operator (operator)
import Endomorph.Token (Token(EndOfInput))

lex :: String -> Either (ParseErrorBundle String Void) [Token]
lex = runParser (tokens <* endOfInput) "stdin"

tokens :: Parser [Token]
tokens = many . lexeme $ token

token :: Parser Token
token = choice
  [ identifier
  , literal
  , operator ]

endOfInput :: Parser Token
endOfInput = do
  eof
  return EndOfInput

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

lexeme :: Parser Token -> Parser Token
lexeme = L.lexeme spacesAndComments

symbol :: String -> Parser String
symbol = L.symbol spacesAndComments

spacesAndComments :: Parser ()
spacesAndComments = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")
