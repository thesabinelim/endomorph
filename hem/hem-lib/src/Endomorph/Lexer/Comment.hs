module Endomorph.Lexer.Comment where

import Endomorph.Lexer.Common (Parser)
import Endomorph.Lexer.Util (isLineBreakChar)
import Endomorph.Token (Token (..))
import Text.Megaparsec
  ( anySingle,
    choice,
    manyTill,
    takeWhileP,
  )
import Text.Megaparsec.Char (string)

comment :: Parser Token
comment =
  choice
    [ multiLineComment,
      singleLineComment
    ]

multiLineComment :: Parser Token
multiLineComment = do
  _ <- string "/*"
  text <- manyTill anySingle $ string "*/"
  return $ Comment text

singleLineComment :: Parser Token
singleLineComment = do
  _ <- string "//"
  text <- takeWhileP (Just "non-line break character") $ not . isLineBreakChar
  return $ Comment text
