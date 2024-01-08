module Endomorph.Lexer.Identifier where

import Control.Applicative (optional)
import Data.Maybe (fromMaybe)
import Endomorph.Lexer.Common (Parser)
import Endomorph.Token (Token (Identifier))
import qualified Text.Megaparsec as M (choice, many)
import qualified Text.Megaparsec.Char as C (alphaNumChar, char, letterChar)

identifier :: Parser Token
identifier = do
  first <-
    M.choice
      [ C.letterChar,
        C.char '_'
      ]
  rest <-
    optional . M.many $
      M.choice
        [ C.alphaNumChar,
          C.char '_'
        ]
  return $ Identifier $ first : fromMaybe [] rest
