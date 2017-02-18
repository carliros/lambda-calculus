module Main where

import           Ast
import           Control.Monad
import           Control.Monad.Trans
import           Parser
import           System.Console.Haskeline
import           System.Environment
import           System.IO

main :: IO ()
main = runInputT defaultSettings loop
  where loop = do minput <- getInputLine "untyped> "
                  case minput of
                    Nothing -> loop
                    Just str -> case str of
                                  ""   -> loop
                                  ":q" -> outputStrLn "Goodbye."
                                  _    -> (liftIO $ process str) >> loop

        process :: String -> IO ()
        process input = putStrLn $ show (parse input)
