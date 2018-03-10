module Constraints where

import Data.Char
import Data.List.Tree
import Data.Maybe
import Data.List


--x = branchAndBound x y


meetsHardConstr :: [(Char,Char)] -> [(Char,Char)] -> [(Char,Char)] -> [Char] -> Bool

meetsHardConstr fpa fm tnt state = iterState fpa fm state && meetsTnt tnt state

-- takes the fpa, fm, and the state
iterState :: [(Char,Char)] -> [(Char,Char)] -> [Char] -> Bool

iterState fpa fm [] = True
iterState fpa fm all@(t:tasks) = meetsFpa fpa (fromJust $ elemIndex t all , t) && meetsFm fm (fromJust $ elemIndex t all , t) && iterState fpa fm tasks

-- takes a list of pairs (mach, task) from forced partial assignment input
-- and the current pair (mach,task) and returns a Bool
meetsFpa :: [(Char,Char)] -> (Int,Char) -> Bool

meetsFpa [] pair = False
meetsFpa ((m,t):pairs) (mach,task)
  | (ord m - 48) == mach && t == task = True
  | (ord m - 48) /= mach || t /= task = meetsFpa pairs (mach,task)


-- takes a list of pairs (mach, task) from forbidden machine input
-- and the current pair (mach,task) and returns a Bool
meetsFm :: [(Char,Char)] -> (Int, Char) -> Bool

meetsFm [] pair = True
meetsFm ((m,t):pairs) (mach,task)
  | (ord m - 48) == mach && t == task = False
  | (ord m - 48) /= mach || t /= task = meetsFpa pairs (mach,task)


-- takes a list of pairs and the state and returns a boolean
meetsTnt :: [(Char,Char)] -> [Char] -> Bool

-- takes a list of pairs of tasks and a tuple of two nearby tasks and returns a boolean
iterTnt :: [(Char, Char)] -> (Char, Char) -> Bool

meetsTnt list [] = True
meetsTnt list (t1 : t2 : tasks) = iterTnt list (t1,t2) && meetsTnt list (t2 : tasks)

--meetsTnt list ((m1,t1) : (m2,t2) : pairs) = iterTnt list (t1,t2) && meetsTnt list ((m2,t2) : pairs)

iterTnt [] (t1,t2) = True
iterTnt ((t1, t2):pairs) (t1', t2')
  | t1 == t1' && t2 == t2' = False
  | otherwise = iterTnt pairs (t1', t2')


-- returns the total penalty value so far
calcPenalty :: [[Int]] -> [(Char, Char)] -> Int

calcPenalty list state = 0

calcMp :: Int -> [[Int]] -> [Char] -> Int

calcMp n mp state
  | state !! n == 'X' = 0
  | otherwise = mp !! n !! (ord (state !! n ) - 65)


-- takes a list of triplets (tnp) and the state and returns the penalty value
calcTnp :: [(Char, Char, Int)] -> [Char] -> Int

calcTnp list [] = 0
calcTnp list ('X' : tasks) = 0
calcTnp list (t1 : t2 : tasks) = iterTnp list (t1,t2) + calcTnp list (t2 : tasks)

iterTnp :: [(Char,Char,Int)] -> (Char,Char) -> Int

iterTnp ((t1, t2, p):triplets) (t1', t2')
  | t1 == t1' && t2 == t2' = p
  | otherwise = iterTnp triplets (t1', t2')



