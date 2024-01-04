module Main where

import Control.Monad.Trans (liftIO)
import Endomorph.Lexer (lex)
import System.Console.Haskeline (defaultSettings, getInputLine, runInputT)
import Prelude hiding (lex)

process :: String -> IO ()
process line = case lex line of
  Left _ -> print "Error"
  Right tokens -> print tokens

main :: IO ()
main = do
  putStrLn "Endomorph 1.0.0"
  runInputT defaultSettings loop
  where
    loop = do
      maybeLine <- getInputLine "> "
      case maybeLine of
        Nothing -> return ()
        Just line -> liftIO (process line) >> loop
