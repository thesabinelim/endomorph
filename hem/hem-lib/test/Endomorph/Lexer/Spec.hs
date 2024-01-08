module Endomorph.Lexer.Spec where

import Endomorph.Lexer (lex)
import Endomorph.Token (Operator (..), Token (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude hiding (lex)

lexerSpec :: Spec
lexerSpec = do
  describe "Lexer" $ do
    it "lexes a sequence of = characters" $ do
      lex "======="
        `shouldBe` Right
          [ Operator Equality,
            Operator Equality,
            Operator Equality,
            Operator Assign,
            EndOfInput
          ]
