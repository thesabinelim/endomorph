module Main where

import Endomorph.Lexer.Spec (lexerSpec)
import Test.Hspec (describe, hspec)

main :: IO ()
main = hspec $ do
  describe "Endomorph" $ do
    lexerSpec
