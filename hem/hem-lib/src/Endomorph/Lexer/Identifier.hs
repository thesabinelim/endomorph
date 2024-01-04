module Endomorph.Lexer.Identifier where

import Control.Applicative (optional)
import Data.Maybe (fromMaybe)
import Endomorph.Lexer.Common (Parser)
import Endomorph.Token (Token (Identifier))
import Text.Megaparsec (choice, many)
import Text.Megaparsec.Char (alphaNumChar, char, letterChar)

identifier :: Parser Token
identifier = do
  first <-
    choice
      [ letterChar,
        char '_'
      ]
  rest <-
    optional . many $
      choice
        [ alphaNumChar,
          char '_'
        ]
  return $ Identifier $ first : fromMaybe [] rest
