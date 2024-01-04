module Main where

import Endomorph.Spec (endomorphSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  endomorphSpec
