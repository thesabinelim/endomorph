module Endomorph.Lexer.Util where

import Data.Foldable (asum)
import Control.Applicative (Alternative)
import Text.Megaparsec.Char (string)

import Endomorph.Lexer.Common (Parser)
import Endomorph.Token (Token)

symbolChoice :: (Foldable t, Functor t) => t (String, Token) -> Parser Token
symbolChoice choices = asum $ fmap getToken choices
  where
    getToken (sequence, token) = symbol sequence token

symbol :: String -> Token -> Parser Token
symbol sequence token = do
  string sequence
  return token
