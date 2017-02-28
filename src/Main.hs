module Main where

import           Ast
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.Map.Strict          as Map
import           Eval
import           Parser
import           Preload
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
        process input = do let expr = parse input
                           let result = sem_Root (Root expr) definitions
                           print result
