module Endomorph.Lexer.Punctuator where

import Endomorph.Lexer.Common (Parser)
import Endomorph.Lexer.Util (symbolChoice)
import Endomorph.Token (Punctuator (..), Token (Punctuator))
import Text.Megaparsec.Char (char, string)

punctuator :: Parser Token
punctuator =
  symbolChoice
    [ (":", Punctuator Colon),
      (",", Punctuator Comma),
      ("{", Punctuator LBrace),
      ("[", Punctuator LBracket),
      ("(", Punctuator LParens),
      ("}", Punctuator RBrace),
      ("]", Punctuator RBracket),
      (")", Punctuator RParens)
    ]
