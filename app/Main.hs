module Main where

import System.Environment
import Data.List
import Control.Monad
import System.IO

main :: IO ()
main = do
  (x:y:args) <- getArgs
  let sizeArgs = length args
  handle <- openFile x ReadMode
  contents <- hGetContents handle
  let strings = lines contents
--writeFile y (parser strings)
  writeFile y (last strings)
