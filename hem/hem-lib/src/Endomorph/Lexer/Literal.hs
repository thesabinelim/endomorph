module Endomorph.Lexer.Literal where

import Data.Char (isDigit, isHexDigit, isOctDigit)
import Endomorph.Lexer.Common (Parser)
import Endomorph.Token (Token)
import qualified Text.Megaparsec as M (takeWhile1P)

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
binaryDigits = M.takeWhile1P (Just "binary digit") isBinaryDigit

octalDigits :: Parser String
octalDigits = M.takeWhile1P (Just "octal digit") isOctDigit

decimalDigits :: Parser String
decimalDigits = M.takeWhile1P (Just "digit") isDigit

hexDigits :: Parser String
hexDigits = M.takeWhile1P (Just "hexadecimal digit") isHexDigit
