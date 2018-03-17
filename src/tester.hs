module Main where

import Data.List
import Constraints
import Bnb

main = do
  let maxInt = maxBound :: Int

{-
  let con = Constraints.ConstraintTup [] [] [] (replicate 8 [1..8]) []
  print (Bnb.branch [] con maxInt ['A','B','C','D','E','F','G','H'])
-}
  let con = Constraints.ConstraintTup [(0,'A')] [] [] (replicate 8 [1..8]) []
  print (Bnb.branch (permutations['A','B','C','D','E','F','G','H']) con)

{-
  let con = Constraints.ConstraintTup [] [] [] (replicate 8 [1..8]) []
  print (Bnb.branch [] con maxInt ['A','B','C','D','E','F','G','H'])
-}
{-
  let con = Constraints.ConstraintTup [] [] [] (replicate 8 [1..8]) [] _
  if fpa con == [] && fm con == [] && tnt con == [] && mp con == (replicate 8 [1..8]) && tnp con == []
    then putStrLn "True"
    else putStrLn "False"
  let con = Constraints.ConstraintTup [(0,'A')] [(1,'B')] [('B','C')] (replicate 8 [1..8]) [('H','A',10)] _
  if fpa con == [(0,'A')] && fm con == [(1,'B')] && tnt con == [('B','C')] && mp con == (replicate 8 [1..8]) && tnp con == [('H','A',10)]
    then putStrLn "True"
    else putStrLn "False"
  let con = Constraints.ConstraintTup [(0,'A'),(2,'B')] [(1,'B'),(3,'C')] [('B','C'),('C','D')] (replicate 8 [1..8]) [('H','A',10),('H','A',10)] _
  if fpa con == [(0,'A'),(2,'B')] && fm con == [(1,'B'),(3,'C')] && tnt con == [('B','C'),('C','D')] && mp con == (replicate 8 [1..8]) && tnp con == [('H','A',10),('H','A',10)]
    then putStrLn "True"
    else putStrLn "False"
-- }

  let con = Constraints.ConstraintTup [(0,'A')] [(1,'B')] [('B','C')] (replicate 8 [1..8]) [('H','A',10)] _
  putStr "Should be False, and is "
  if Constraints.meetsFpa (fpa con) (1,'A')
    then putStrLn "True"
    else putStrLn "False"
  putStr "Should be False, and is "
  if Constraints.meetsFpa (fpa con) (0,'B')
    then putStrLn "True"
    else putStrLn "False"
  putStr "Should be True, and is "
  if Constraints.meetsFpa (fpa con) (1,'C')
    then putStrLn "True"
    else putStrLn "False"
  putStr "Should be True, and is "
  if Constraints.meetsFpa (fpa con) (0,'A')
    then putStrLn "True\n"
    else putStrLn "False\n"

  putStr "Should be True, and is "
  if Constraints.meetsFm (fm con) (1,'A')
    then putStrLn "True"
    else putStrLn "False"
  putStr "Should be True, and is "
  if Constraints.meetsFm (fm con) (0,'B')
    then putStrLn "True"
    else putStrLn "False"
  putStr "Should be False, and is "
  if Constraints.meetsFm (fm con) (1,'B')
    then putStrLn "True"
    else putStrLn "False"
-}
