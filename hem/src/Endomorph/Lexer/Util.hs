module Endomorph.Lexer.Util where

import Text.Megaparsec.Char (string)

import Endomorph.Lexer.Common (Parser)
import Endomorph.Token (Token)

symbol :: String -> Token -> Parser Token
symbol symbol token = do
  string symbol
  return token
