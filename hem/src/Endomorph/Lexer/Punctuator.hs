module Endomorph.Lexer.Punctuator where

import Endomorph.Lexer.Common (Parser)
import Endomorph.Lexer.Util (symbolToToken)
import Endomorph.Token (Punctuator (..), Token (Punctuator))

punctuator :: Parser Token
punctuator =
  symbolToToken
    [ (":", Punctuator Colon),
      (",", Punctuator Comma),
      ("{", Punctuator LBrace),
      ("[", Punctuator LBracket),
      ("(", Punctuator LParens),
      ("}", Punctuator RBrace),
      ("]", Punctuator RBracket),
      (")", Punctuator RParens)
    ]
