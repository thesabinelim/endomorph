module Endomorph.Lexer where

import Data.Char (isSpace)
import Data.Void (Void)
import Endomorph.Lexer.Comment (comment)
import Endomorph.Lexer.Common (Parser)
import Endomorph.Lexer.Identifier (identifier)
import Endomorph.Lexer.Literal (literal)
import Endomorph.Lexer.Operator (operator)
import Endomorph.Lexer.Punctuator (punctuator)
import Endomorph.Token (Token (End, Whitespace))
import Text.Megaparsec
  ( ParseErrorBundle,
    choice,
    eof,
    many,
    runParser,
    takeWhile1P,
  )

lex :: String -> Either (ParseErrorBundle String Void) [Token]
lex = runParser tokensTillEnd "stdin"

tokensTillEnd :: Parser [Token]
tokensTillEnd = do
  tokens <- many token
  eoi <- endOfInput
  return $ tokens ++ [eoi]

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

end :: Parser Token
end = do
  eof
  return End

whitespace :: Parser Token
whitespace = do
  spaces <- takeWhile1P (Just "whitespace") isSpace
  return $ Whitespace spaces
