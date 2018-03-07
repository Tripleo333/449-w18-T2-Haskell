module Main where

import System.Environment
import Data.List

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      x <- readFile file
      putStr x
    _ -> putStrLn "Wrong number of arguments"
