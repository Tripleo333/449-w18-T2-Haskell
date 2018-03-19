module Example where

import Constraints
import Data.List

removeLetter :: [Char] -> Char -> [Char]
removeLetter x y = [a | a <- x, a /= y]

-- Takes split string, removes thing at index, adds thing at index, joins strings.
replaceIndex :: ([Char], [Char]) -> Char -> [Char]
replaceIndex (x,_:ys) thing = x ++ thing:ys

-- List of all possible chars (should be A,B,C,D,E,F,G,H, Anything longer than this takes a LONG time), returns all enumerations
allStatesEnum :: [Char] -> [[Char]]
allStatesEnum x = permutations x

branchEnum :: [[Char]] -> Constraints.ConstraintTup -> [Char]
branchEnum s c = getReturn [x | x <- s, meetsHardConstr (fPA c) (fM c) (tNt c) x] c

-- State -> Length (index of next X) -> Constraints -> Current Global Minimum -> Chars not yet used in State -> Final State
branch :: [Char] -> Int -> Constraints.ConstraintTup -> Int -> [Char] -> [Char]
branch w 8 y z a = w
branch w x y z a
    -- If the penalty value of state is >= current global minimum OR it does not meet hard constraints, return char list containing only '0'
    | not ( meetsHardConstr (fPA y) (fM y) (tNt y) w ) = "XXXXXXXX"
    -- Get the lowest penalty value of list of branch where each branch adds a char to the next index from a(chars not used yet) and passes it length + 1, and removes the used letter from a
    | otherwise = getReturn [branch (replaceIndex(splitAt x w) b) (x + 1) y z (removeLetter a b) | b <- a] y



getReturn :: [[Char]] -> Constraints.ConstraintTup -> [Char]
getReturn (x:xs:xss) y
    | x == "XXXXXXXX" = getReturn (xs:xss) y
    | ( calcPenalty (mP y) (tNP y) x ) < ( calcPenalty (mP y) (tNP y) ( getReturn (xs:xss) y ) ) = x
    | otherwise = getReturn (xs:xss) y
getReturn (x:xs:[]) y
    | x == "XXXXXXXX" = getReturn [xs] y
    | ( calcPenalty (mP y) (tNP y) x ) < ( calcPenalty (mP y) (tNP y) ( getReturn [xs] y ) ) = x
    | otherwise = getReturn [xs] y
getReturn x y = x !! 0