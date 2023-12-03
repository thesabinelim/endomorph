module Endomorph.Lexer.Punctuation where

import Endomorph.Lexer.Common (Parser)
import Endomorph.Lexer.Util (symbolChoice)
import Endomorph.Token (Punctuation (..), Token (Punctuation))
import Text.Megaparsec.Char (char, string)

punctuation :: Parser Token
punctuation =
  symbolChoice
    [ (":", Punctuation Colon),
      (",", Punctuation Comma),
      ("{", Punctuation LBrace),
      ("[", Punctuation LBracket),
      ("(", Punctuation LParens),
      ("}", Punctuation RBrace),
      ("]", Punctuation RBracket),
      (")", Punctuation RParens),
      (";", Punctuation Semicolon)
    ]
