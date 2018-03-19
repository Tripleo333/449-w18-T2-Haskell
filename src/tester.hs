module Main where

import Data.List
import Constraints
import Example
import Total

main = do
  let con = total ["Name:","invalidforbidden",[],"forced partial assignment:",[],"forbidden machine:","(1,A)","(2,A)","(3,A)","(4,A)","(5,A)","(6,A)","(7,A)","(8,A)",[],"too-near tasks:",[],"machine penalties:","1 1 1 1 1 1 1 1","1 1 1 1 1 1 1 1","1 1 1 1 1 1 1 1","1 1 1 1 1 1 1 1","1 1 1 1 1 1 1 1","1 1 1 1 1 1 1 1","1 1 1 1 1 1 1 1","1 1 1 1 1 1 1 1",[],"too-near penalities"] 0 0 (ConstraintTup [] [] [] [] [] [])
  print con --(unlines (Constraints.error con))
{-
  let con = ConstraintTup [] [] [] [[]] [] []
  let fpa = [(0,'A')]
  let fPA con = (fPA con) ++ fpa
  print con

  let con = total ["Name:","machpen2",[],"forced partial assignment:","(1,A)","(2,B)","(3,C)","(4,D)","(5,E)",[],"forbidden machine:","(7,F)",[],"too-near tasks:","(E,F)","(F,E)",[],"machine penalties","1 1 1 1 1 1 1 1","1 1 1 1 1 1 1 1","1 1 1 1 1 1 1 1","1 1 1 1 1 1 1 1","1 1 1 1 1 1 1 1","1 1 1 1 1 1 10 20","1 1 1 1 1 1 10 30","1 1 1 1 1 10 1 1",[],"too-near penalities"] 0 0 (ConstraintTup [] [] [] [[]] [] [])
  print (Example.branch ['X','X','X','X','X','X','X','X'] 0 con 0 ['A'..'H'])


  let con = Constraints.ConstraintTup [] [] [] (replicate 8 [1..8]) [] []  
--  print (Example.getReturn [['A'..'H'] ++ [replicate 7 ['X','X','X','X','X','X','X','X']]] con)


  let con = Constraints.ConstraintTup [] [] [] (replicate 8 [1..8]) [] []  
  print (Example.branch ['X','X','X','X','X','X','X','X'] 0 con 0 ['A'..'H'])
  let con = Constraints.ConstraintTup [(0,'A')] [] [] (replicate 8 [1..8]) [] []  
  print (Example.branch ['X','X','X','X','X','X','X','X'] 0 con 0 ['A'..'H'])
  let con = Constraints.ConstraintTup [] [(0,'A')] [] (replicate 8 [1..8]) [] []  
  print (Example.branch ['X','X','X','X','X','X','X','X'] 0 con 0 ['A'..'H'])
  let con = Constraints.ConstraintTup [] [(0,'H')] [] (replicate 8 [1..8]) [] []  
  print (Example.branch ['X','X','X','X','X','X','X','X'] 0 con 0 ['A'..'H'])
  let con = Constraints.ConstraintTup [] [] [('H','G')] (replicate 8 [1..8]) [] []  
  print (Example.branch ['X','X','X','X','X','X','X','X'] 0 con 0 ['A'..'H'])


  print (meetsHardConstr [(0,'A')] [] [] ['X','X','X','X','X','X','X','X'])
  print (meetsHardConstr [] [] [] ['X','X','X','X','X','X','X','X'])

  putStr "Should be True "
  print (meetsHardConstr [(0,'A')] [] [] ['A'..'H'])
  putStr "Should be False "
  print (meetsHardConstr [(0,'B')] [] [] ['A'..'H'])
  putStr "Should be True "
  print (meetsHardConstr [(1,'B')] [] [] ['A'..'H'])
  putStr "Should be False "
  print (meetsHardConstr [(1,'A')] [] [] ['A'..'H'])
  putStr "Should be True "
  print (meetsHardConstr [(7,'H')] [] [] ['A'..'H'])
  putStr "Should be False "
  print (meetsHardConstr [] [(0,'A')] [] ['A'..'H'])


  let con = Constraints.ConstraintTup [] [] [] (replicate 8 [1..8]) []
  print (Example.branch [] con maxInt ['A','B','C','D','E','F','G','H'])

  let con = Constraints.ConstraintTup [(0,'A')] [] [] (replicate 8 [1..8]) []
  print (Example.branch (permutations['A','B','C','D','E','F','G','H']) con)

  let con = Constraints.ConstraintTup [] [] [] (replicate 8 [1..8]) []
  print (Example.branch [] con maxInt ['A','B','C','D','E','F','G','H'])
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