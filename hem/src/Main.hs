module Main where

import Control.Monad.Trans (liftIO)
import Endomorph.Lexer (lex)
import System.Console.Haskeline (defaultSettings, getInputLine, runInputT)
import System.Exit (exitSuccess)
import System.IO (hFlush, isEOF, stdout)
import Prelude hiding (lex)

process :: String -> IO ()
process line = case lex line of
  Left a -> print "Error"
  Right a -> print a

main :: IO ()
main = do
  putStrLn "Endomorph 1.0.0"
  runInputT defaultSettings loop
  where
    loop = do
      line <- getInputLine "> "
      case line of
        Nothing -> return ()
        Just line -> liftIO (process line) >> loop
