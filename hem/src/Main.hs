module Main where

import Control.Monad (unless)
import System.Exit (exitSuccess)
import System.IO (hFlush, isEOF, stdout)

inputLoop :: IO ()
inputLoop = do
  putStr "> "
  hFlush stdout
  done <- isEOF
  unless done $ do
    line <- getLine
    inputLoop

main :: IO ()
main = do
  putStrLn "Endomorph 1.0.0"
  inputLoop
