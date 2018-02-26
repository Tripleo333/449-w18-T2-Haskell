module Testy
( fun
, sequenceSum
) where

import Data.List (intercalate)

fun :: Int -> Int
fun a = a^2

sequenceSum :: Int -> String
sequenceSum x
  | x < 0 = show x ++ " < 0"
  | otherwise = intercalate "+" (map show [0..x]) ++ " = " ++ show (sum [0..x])
--  | otherwise = show (div (x*(x+1)) 2)
