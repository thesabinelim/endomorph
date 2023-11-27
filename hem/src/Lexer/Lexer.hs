module Lexer.Lexer where

import Control.Applicative (optional)
import Control.Monad.State (evalState, get, put, State)
import Data.Char (isDigit, isHexDigit, isOctDigit)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec
  ( between
  , choice
  , eof
  , many
  , Parsec
  , ParseErrorBundle
  , runParser
  , takeWhile1P )
import Text.Megaparsec.Char
  ( char
  , letterChar
  , space1
  , string
  , alphaNumChar )
import qualified Text.Megaparsec.Char.Lexer as L
  ( decimal
  , float
  , lexeme
  , signed
  , skipLineComment
  , skipBlockComment
  , space
  , symbol )

import Token (Token(..), Operator(..))

lex :: String -> Either (ParseErrorBundle String Void) [Token]
lex = runParser (tokens <* endOfInput) "stdin"

type Parser = Parsec Void String

tokens :: Parser [Token]
tokens = many token

token :: Parser Token
token = choice
  [ identifier
  , literal
  , operator ]

endOfInput :: Parser Token
endOfInput = do
  eof
  return EndOfInput

identifier :: Parser Token
identifier = lexeme $ do
  first <- choice
    [ letterChar
    , char '_' ]
  rest <- optional . many $ choice 
    [ alphaNumChar
    , char '_' ]
  return $ Identifier $ first : fromMaybe [] rest

{-
  Literals
-}

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

{-
  Operators
-}

operator :: Parser Token
operator = lexeme $ choice
  [ eqEq
  , gtEq
  , lshift
  , ltEq
  , pow
  , rshift
  , Lexer.Lexer.and
  , Lexer.Lexer.div
  , eq
  , gt
  , lt
  , Lexer.Lexer.min
  , Lexer.Lexer.mod
  , mult
  , Lexer.Lexer.not
  , Lexer.Lexer.or
  , plus ]

and :: Parser Token
and = do
  char '&'
  return $ Operator And

div :: Parser Token
div = do
  char '/'
  return $ Operator Div

eqEq :: Parser Token
eqEq = do
  string "=="
  return $ Operator EqEq

eq :: Parser Token
eq = do
  char '='
  return $ Operator Eq

gt :: Parser Token
gt = do
  char '>'
  return $ Operator Gt

gtEq :: Parser Token
gtEq = do
  string ">="
  return $ Operator GtEq

lshift :: Parser Token
lshift = do
  string "<<"
  return $ Operator Lshift

lt :: Parser Token
lt = do
  char '<'
  return $ Operator Lt

ltEq :: Parser Token
ltEq = do
  string "<="
  return $ Operator LtEq

min :: Parser Token
min = do
  char '-'
  return $ Operator Min

mod :: Parser Token
mod = do
  char '%'
  return $ Operator Mod

mult :: Parser Token
mult = do
  char '*'
  return $ Operator Mult

not :: Parser Token
not = do
  char '!'
  return $ Operator Not

or :: Parser Token
or = do
  char '|'
  return $ Operator Or

plus :: Parser Token
plus = do
  char '+'
  return $ Operator Plus

pow :: Parser Token
pow = do
  string "**"
  return $ Operator Pow

rshift :: Parser Token
rshift = do
  string ">>"
  return $ Operator Rshift

xor :: Parser Token
xor = do
  char '^'
  return $ Operator Xor

{-
  Helpers
-}

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
