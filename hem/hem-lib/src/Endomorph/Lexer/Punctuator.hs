module Endomorph.Lexer.Punctuator where

import Endomorph.Lexer.Common (Parser)
import Endomorph.Lexer.Util (stringToToken)
import Endomorph.Token (Punctuator (..), Token (Punctuator))

punctuator :: Parser Token
punctuator =
  stringToToken
    [ (":", Punctuator Colon),
      (",", Punctuator Comma),
      ("{", Punctuator LBrace),
      ("[", Punctuator LBracket),
      ("(", Punctuator LParens),
      ("}", Punctuator RBrace),
      ("]", Punctuator RBracket),
      (")", Punctuator RParens)
    ]
