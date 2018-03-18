module Constraints where

import Data.Char
import Data.List

-- constraints data type
data ConstraintTup = ConstraintTup {
  fPA :: [(Int,Char)],
  fM :: [(Int, Char)],
  tNt :: [(Char,Char)],
  mP :: [[Int]],
  tNP :: [(Char, Char, Int)]
--  error :: [String]
} deriving (Show)


-- returns True if the state meets all hard constraints, returns False if the state does not meet
-- at least one of the hard constraints
meetsHardConstr :: [(Int,Char)] -> [(Int,Char)] -> [(Char,Char)] -> [Char] -> Bool
meetsHardConstr fpa fm tnt "XXXXXXXX" = True
meetsHardConstr fpa fm tnt state = iterState fpa fm state && meetsTnt tnt state


-- takes the fpa, fm, and the state
iterState :: [(Int,Char)] -> [(Int,Char)] -> [Char] -> Bool
iterState fpa fm [] = True
iterState fpa fm state = and [meetsFpa fpa x | x <- (zip [0..7] state)] && and [meetsFm fm x | x <- (zip [0..7] state)]


-- takes a list of pairs (mach, task) from forced partial assignment input
-- and the current pair (mach,task) and returns a Bool
meetsFpa :: [(Int,Char)] -> (Int,Char) -> Bool
meetsFpa [] pair = True
meetsFpa x (_,'X') = True
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
meetsTnt [] _ = True
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


-- returns the total penalty value so far (for the state passed in as parameter)
calcPenalty :: [[Int]] -> [(Char, Char, Int)] -> [Char] -> Int
calcPenalty mp tnp "XXXXXXXX" = maxBound :: Int
calcPenalty mp tnp state = calcMp mp state + calcTnp tnp state

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
-- pass around the first task to compare it to the last
calcTnp list all@(t1 : tasks) = calcTnp1 list all t1

-- calculates the too-near penalty associated with the given state and tnp
calcTnp1 list [] t0 = 0
calcTnp1 list [t] t0 = iterTnp list (t,t0) -- + iterTnp list (t0,t)
calcTnp1 list ('X' : tasks) t0 = calcTnp1 list tasks t0
calcTnp1 list (t1 : t2 : tasks) t0 = iterTnp list (t1,t2) + calcTnp1 list (t2 : tasks) t0

-- returns the penalty associated with the given tnp and the pair of neighboring tasks
iterTnp :: [(Char,Char,Int)] -> (Char,Char) -> Int
iterTnp [] pair = 0
iterTnp ((t1, t2, p):triplets) (t1', t2')
  | t1 == t1' && t2 == t2' = p
  | otherwise = iterTnp triplets (t1', t2')
