module Endomorph.Spec where

import Endomorph.Lexer.Spec (lexerSpec)
import Test.Hspec (Spec, describe)

endomorphSpec :: Spec
endomorphSpec = describe "Endomorph" $ do
  lexerSpec
