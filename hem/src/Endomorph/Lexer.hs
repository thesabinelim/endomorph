module Endomorph.Lexer where

import Data.Char (isSpace)
import Data.Void (Void)
import Endomorph.Lexer.Common (Parser)
import Endomorph.Lexer.Identifier (identifier)
import Endomorph.Lexer.Literal (literal)
import Endomorph.Lexer.Operator (operator)
import Endomorph.Lexer.Punctuation (punctuation)
import Endomorph.Lexer.Util (isLineBreakChar)
import Endomorph.Token (Token (Comment, EndOfInput, Whitespace))
import Text.Megaparsec
  ( ParseErrorBundle,
    Parsec,
    choice,
    eof,
    many,
    runParser,
    takeWhile1P,
    takeWhileP,
  )
import Text.Megaparsec.Char (char, space1)

lex :: String -> Either (ParseErrorBundle String Void) [Token]
lex = runParser (tokens <* endOfInput) "stdin"

tokens :: Parser [Token]
tokens = many token

token :: Parser Token
token =
  choice
    [ comment,
      identifier,
      literal,
      operator,
      punctuation,
      whitespace
    ]

endOfInput :: Parser Token
endOfInput = do
  eof
  return EndOfInput

comment :: Parser Token
comment = do
  char '#'
  text <- takeWhileP (Just "text") $ not . isLineBreakChar
  return $ Comment text

whitespace :: Parser Token
whitespace = do
  spaces <- takeWhile1P (Just "whitespace") isSpace
  return $ Whitespace spaces
