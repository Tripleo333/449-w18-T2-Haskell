module Main where

import System.Environment
import Data.List
import Control.Monad
import System.IO
import Total
import Constraints
import Test
import Example

main :: IO ()
main = do
  (x:y:args) <- getArgs
--  let sizeArgs = length args
  handle <- openFile x ReadMode
  contents <- hGetContents handle
  let strings = lines contents
  let parsedConsTup = (total strings 0 0 (Constraints.ConstraintTup [] [] [] [] [] []))

  writeFile y ((outPut parsedConsTup)++"\n")

outPut :: ConstraintTup -> String
outPut con
  | length (Constraints.error con) /= 0 = (outPut' (head (Constraints.error con)))
  | otherwise = (branch "XXXXXXXX" 0 con 0 "ABCDEFGH")

outPut' :: String -> String
outPut' "XXXXXXXX" = "No valid solution possible!"
outPut' str = str


{-
outPut :: ConstraintTup -> String
outPut con = outPut' con (branch "XXXXXXXX" 0 con 0 "ABCDEFGH")

outPut' :: ConstraintTup -> String -> String
outPut' con str
  | length == 0 = "No valid solution possible!"
  | otherwise = str
-}