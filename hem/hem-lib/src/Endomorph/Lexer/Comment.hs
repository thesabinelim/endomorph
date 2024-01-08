module Endomorph.Lexer.Comment where

import Endomorph.Lexer.Common (Parser)
import Endomorph.Lexer.Util (isLineBreakChar)
import Endomorph.Token (Token (..))
import qualified Text.Megaparsec as M
  ( anySingle,
    choice,
    manyTill,
    takeWhileP,
  )
import qualified Text.Megaparsec.Char as C (string)

comment :: Parser Token
comment =
  M.choice
    [ multiLineComment,
      singleLineComment
    ]

multiLineComment :: Parser Token
multiLineComment = do
  _ <- C.string "/*"
  text <- M.manyTill M.anySingle $ C.string "*/"
  return $ Comment text

singleLineComment :: Parser Token
singleLineComment = do
  _ <- C.string "//"
  text <- M.takeWhileP (Just "non-line break character") $ not . isLineBreakChar
  return $ Comment text
