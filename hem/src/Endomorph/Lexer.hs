module Endomorph.Lexer where

import Data.Char (isSpace)
import Data.Void (Void)
import Endomorph.Lexer.Comment (comment)
import Endomorph.Lexer.Common (Parser)
import Endomorph.Lexer.Identifier (identifier)
import Endomorph.Lexer.Literal (literal)
import Endomorph.Lexer.Operator (operator)
import Endomorph.Lexer.Punctuator (punctuator)
import Endomorph.Token (Token (EndOfInput, Whitespace))
import Text.Megaparsec
  ( ParseErrorBundle,
    choice,
    eof,
    many,
    runParser,
    takeWhile1P,
  )
import Text.Megaparsec.Char (space1, string)

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
      punctuator,
      whitespace
    ]

endOfInput :: Parser Token
endOfInput = do
  eof
  return EndOfInput

whitespace :: Parser Token
whitespace = do
  spaces <- takeWhile1P (Just "whitespace") isSpace
  return $ Whitespace spaces
