module Constraints where

import Data.Ord
import Data.List.Tree


--x = branchAndBound x y


meetsHardConstr :: [(Char,Char)] -> [(Char,Char)] -> [(Char,Char)] -> [(Char,Char)] -> Bool

meetsHardConstr fpa fm tnt state = iterState fpa fm state && meetsTnt tnt state


iterState :: [(Char,Char)] -> [(Char,Char)] -> [(Char,Char)] -> Bool

iterState fpa fm [] = True
iterState fpa fm ((m,t):pairs) = meetsFpa fpa (m,t) && meetsFm fm (m,t) && iterState fpa fm pairs

-- takes a list of pairs (mach, task) from forced partial assignment input
-- and the current pair (mach,task) and returns a Bool
meetsFpa :: [(Char,Char)] -> (Char, Char) -> Bool

meetsFpa [] (x,y) = False
meetsFpa ((a,b):pairs) (x,y)
  | a == b && b == y = True
  | a /= b || b /= y = meetsFpa pairs (x,y)


-- takes a list of pairs (mach, task) from forbidden machine input
-- and the current pair (mach,task) and returns a Bool
meetsFm :: [(Char,Char)] -> (Char, Char) -> Bool

meetsFm [] (x,y) = True
meetsFm ((a,b):pairs) (x,y)
  | a == b && b == y = False
  | a /= b || b /= y = meetsFpa pairs (x,y)


-- takes a list of pairs and the state and returns a boolean
meetsTnt :: [(Char,Char)] -> [(Char,Char)] -> Bool

-- takes a list of pairs of tasks and a tuple of two nearby tasks and returns a boolean
iterTnt :: [(Char, Char)] -> (Char, Char) -> Bool

meetsTnt list [] = True
meetsTnt list ((m1,t1) : (m2,t2) : pairs) = iterTnt list (t1,t2) && meetsTnt list ((m2,t2) : pairs)

iterTnt [] (t1,t2) = True
iterTnt ((t1, t2):pairs) (t1', t2')
  | t1 == t1' && t2 == t2' = False
  | otherwise = iterTnt pairs (t1', t2')


-- takes a 2D list of penalties and the state and returns a Bool
meetsMpen :: [[Int]] -> [(Char, Char)] -> Bool

meetsMpen list state = True
