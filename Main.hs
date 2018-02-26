module Main where

import System.Environment
import Data.List
import Testy

main :: IO ()
main = do
  args <- getArgs
  case args[0] of
    [file] -> do
      x <- readFile file
      writeFile args[1] x
    _ -> putStrLn "Wrong number of arguments"
