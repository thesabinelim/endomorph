module Endomorph.Lexer.Common where

import Data.Void (Void)
import qualified Text.Megaparsec as M (Parsec)

type Parser = M.Parsec Void String
