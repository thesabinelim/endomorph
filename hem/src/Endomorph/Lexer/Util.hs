module Endomorph.Lexer.Util where

import Control.Applicative (Alternative)
import Data.Foldable (asum)
import Endomorph.Lexer.Common (Parser)
import Endomorph.Token (Token)
import Text.Megaparsec.Char (string)

isLineBreakChar :: Char -> Bool
isLineBreakChar c = c == '\n' || c == '\r'

symbolChoice :: (Foldable t, Functor t) => t (String, Token) -> Parser Token
symbolChoice choices = asum $ fmap getToken choices
  where
    getToken (sequence, token) = symbol sequence token

symbol :: String -> Token -> Parser Token
symbol sequence token = do
  string sequence
  return token
