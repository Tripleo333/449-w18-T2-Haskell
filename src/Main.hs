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
  | length (Constraints.error con) /= 0 = (outPut' con (head (Constraints.error con)))
  | otherwise = outPut' con (branch "XXXXXXXX" 0 con 0 "ABCDEFGH")

outPut' :: ConstraintTup -> String -> String
outPut' _ "XXXXXXXX" = "No valid solution possible!"
outPut' con str = ("Solution " ++ ([str !! 0]) ++ " " ++ ([str !! 1]) ++ " " ++ ([str !! 2]) ++ " " ++ ([str !! 3]) ++ " " ++ ([str !! 4]) ++ " " ++ ([str !! 5]) ++ " " ++ ([str !! 6]) ++ " " ++ ([str !! 7]) ++ "; Quality: ") ++ (show (calcPenalty (mP con) (tNP con) str))

{-
outPut :: ConstraintTup -> String
outPut con = outPut' con (branch "XXXXXXXX" 0 con 0 "ABCDEFGH")

outPut' :: ConstraintTup -> String -> String
outPut' con str
  | length == 0 = "No valid solution possible!"
  | otherwise = str
-}