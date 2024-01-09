module Endomorph.Lexer.Util where

import Data.Foldable (asum)
import Endomorph.Lexer.Common (Parser)
import Endomorph.Token (Token)
import qualified Text.Megaparsec.Char as C (string)

isLineBreakChar :: Char -> Bool
isLineBreakChar c = c == '\n' || c == '\r'

stringToToken :: [(String, Token)] -> Parser Token
stringToToken choices = asum $ fmap getToken choices
  where
    getToken (string_, token) = string string_ token

string :: String -> Token -> Parser Token
string string_ token = do
  _ <- C.string string_
  return token
