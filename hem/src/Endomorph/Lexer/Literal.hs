module Endomorph.Lexer.Literal where

import Data.Char (isDigit, isHexDigit, isOctDigit)
import Text.Megaparsec (takeWhile1P)

import Endomorph.Lexer.Common (Parser)
import Endomorph.Token (Token)

literal :: Parser Token
literal = fail "unimplemented"

{-
  Numeric literals
-}

-- numericLiteral :: Parser Token
-- numericLiteral = do
--   prefixZero <- optional $ char '0'
--   case prefixZero of
--     Nothing -> do
--       return 
--     Just '0' -> do
--       optional $ 
--       return 

--   binaryDigits <- binaryDigits
--   decimalDigits <- decimalDigits
--   return digits

isBinaryDigit :: Char -> Bool
isBinaryDigit c = c == '0' || c == '1'

binaryDigits :: Parser String
binaryDigits = takeWhile1P (Just "binary digit") isBinaryDigit

octalDigits :: Parser String
octalDigits = takeWhile1P (Just "octal digit") isOctDigit

decimalDigits :: Parser String
decimalDigits = takeWhile1P (Just "digit") isDigit

hexDigits :: Parser String
hexDigits = takeWhile1P (Just "hexadecimal digit") isHexDigit
