module Constraints where

import Data.Char
import Data.List.Tree
import Data.Maybe
import Data.List

--constraints data type

data ConstraintTup = ConstraintTup {
  fPA :: [(Int,Char)],
  fM :: [(Int, Char)],
  tNt :: [(Char,Char)],
  mP :: [[Int]],
  tNP :: [(Char, Char, Int)],
  error :: [String]
} deriving (Show)


--x = branchAndBound x y

meetsHardConstr :: [(Int,Char)] -> [(Int,Char)] -> [(Char,Char)] -> [Char] -> Bool
meetsHardConstr fpa fm tnt state = iterState fpa fm state && meetsTnt tnt state


-- takes the fpa, fm, and the state
iterState :: [(Int,Char)] -> [(Int,Char)] -> [Char] -> Bool
iterState fpa fm [] = True
iterState fpa fm all@(t:tasks) = meetsFpa fpa (fromJust $ elemIndex t all , t) && meetsFm fm (fromJust $ elemIndex t all , t) && iterState fpa fm tasks


-- takes a list of pairs (mach, task) from forced partial assignment input
-- and the current pair (mach,task) and returns a Bool
meetsFpa :: [(Int,Char)] -> (Int,Char) -> Bool
meetsFpa [] pair = True
meetsFpa ((m,t):pairs) (mach,task)
  | (m == mach && t /= task) || (m /= mach && t == task) = False
  | otherwise = meetsFpa pairs (mach,task)


-- takes a list of pairs (mach, task) from forbidden machine input
-- and the current pair (mach,task) and returns a Bool
meetsFm :: [(Int,Char)] -> (Int, Char) -> Bool
meetsFm [] pair = True
meetsFm ((m,t):pairs) (mach,task)
  | m == mach && t == task = False
  | otherwise = meetsFm pairs (mach,task)


-- takes a list of pairs and the state and returns a boolean
meetsTnt :: [(Char,Char)] -> [Char] -> Bool
meetsTnt list all@(t1 : tasks) = meetsTnt1 list all t1

meetsTnt1 list [] t0 = True
meetsTnt1 list [t] t0 = iterTnt list (t,t0) -- && iterTnt list (t0,t)
meetsTnt1 list (t1 : t2 : tasks) t0 = iterTnt list (t1,t2) && meetsTnt1 list (t2 : tasks) t0


-- takes a list of pairs of tasks and a tuple of two nearby tasks and returns a boolean
iterTnt :: [(Char, Char)] -> (Char, Char) -> Bool
iterTnt [] pair = True
iterTnt ((t1, t2):pairs) (t1', t2')
  | t1 == t1' && t2 == t2' = False
  | otherwise = iterTnt pairs (t1', t2')


-- returns the total penalty value so far
--old single machine calc --calcPenalty :: Int -> [[Int]] -> [(Char, Char, Int)] -> [Char] -> Int
                          --calcPenalty mach mp tnp state = calcMp mach mp state + calcTnp tnp state
calcPenalty :: [[Int]] -> [(Char, Char, Int)] -> [Char] -> Int
calcPenalty mp tnp state = calcMp mp state + calcTnp tnp state

-- old single machine calc -- calcMp :: Int -> [[Int]] -> [Char] -> Int  -- calculates penalty for one machine
                           -- calcMp n mp state
                           --  | state !! n == 'X' = 0
                           --  | otherwise = mp !! n !! (ord (state !! n ) - 65)
-- calculates total Machine Penalty
-- requires full state, filled with 'X's or an actual state
calcMp :: [[Int]] -> [Char] -> Int
calcMp _ [] = 0
calcMp (x:mp) (y:state) 
  | y == 'X' && (length state > 0) = calcMp mp state
  | y == 'X' = 0
  | otherwise = x !! (ord y - 65) + calcMp mp state


-- takes a list of triplets (tnp) and the state and returns the penalty value
calcTnp :: [(Char, Char, Int)] -> [Char] -> Int
calcTnp list all@(t1 : tasks) = calcTnp1 list all t1

calcTnp1 list [] t0 = 0
calcTnp1 list [t] t0 = iterTnp list (t,t0) -- + iterTnp list (t0,t)
calcTnp1 list ('X' : tasks) t0 = calcTnp1 list tasks t0
-- not sure what this was for
                -- calcTnp1 list (a : 'X' : tasks) t0 = calcTnp1 list tasks 
calcTnp1 list (t1 : t2 : tasks) t0 = iterTnp list (t1,t2) + calcTnp1 list (t2 : tasks) t0

iterTnp :: [(Char,Char,Int)] -> (Char,Char) -> Int
iterTnp [] pair = 0
iterTnp ((t1, t2, p):triplets) (t1', t2')
  | t1 == t1' && t2 == t2' = p
  | otherwise = iterTnp triplets (t1', t2')


