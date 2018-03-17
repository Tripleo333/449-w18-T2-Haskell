module Bnb where

import Constraints

{-
--        State -> Constraints -> Current Min -> Unused Letters -> Final State
branch :: [Char] -> Constraints.ContraintTup -> Int -> [Char] -> [Char]
branch s c m u
  | not meetsH && (length u) < 8 = ret
  | (calcPenalty (mp c) (tnp c) s) >= m = ret
  | otherwise = [branch [a | a <- (s !! (length (7 - length u)))
  where meetsH = meetsHardConstr (fpa c) (fm c) (tnt c) s
        ret = removeLetter s (7 - length u)

removeLetter :: [Char] -> Int -> [Char]
removeLetter x y = [x | x !! y = 'X']
-}

{-

-- removes letter from unused letter list
removeLetter :: [Char] -> Char -> [Char]
removeLetter x y = [a | a <- x, a /= y]

-- State -> Length -> Constraints -> Current Global Minimum -> Chars not yet used in State -> Final State
branch :: [Char] -> Int -> Constraints.ConstraintTup -> Int -> [Char] -> [Char]
--branch w [x | x >= 8] y z a
branch w x y z a
    -- If the penalty value of state is >= current global minimum, return char list containing only '0'
--    | Constraints.meetsHardConstr (fpa y) (fm y) (tnt y) w = removeLetter w x
    | (calcPenalty (mp y) (tnp y) w) >= z = ['0']--removeLetter w x
    | otherwise = head [getReturn h y x | h <- [branch (w ++ [b]) x y z (removeLetter a b) | b <- a]]

-- best state at current level??
--           state -> Constraints -> current min -> new state
getReturn :: [Char] -> Constraints.ConstraintTup -> Int -> [Char]
getReturn x y mini = [z | z <- x, (calcPenalty (mp y) (tnp y) x) < mini]
-}

{-
--        state      constraints                 min    unused
branch :: [Char] -> Constraints.ConstraintTup -> Int -> [Char] -> [Char]
branch s c m u
  | not (Constraints.meetsHardConstr (fpa c) (fm c) (tnt c) s) = init s
  | (length s) > 7 && pen > m = init s
  | (length s) > 7 && pen <= m = s
  where pen = (Constraints.calcPenalty (mp c) (tnp c) s)
branch s c m u = [y | y <- [branch (s ++ [x]) c m (removeLetter u x) | x <- u]]

--              unused     toRemove
removeLetter :: [Char] -> Char -> [Char]
removeLetter u r = [x | x <- u, x /= r]
-}
--        state       constraints            
branch :: [[Char]] -> Constraints.ConstraintTup -> (Int, [Char])
--branch s c = do
--  let x = filter (meetsHardConstr (fpa c) (fm c) (tnt c)) s
--  let y = map (calcPenalty (mp c) (tnp c)) x
branch s c = mini(zip (map (calcPenalty (mp c) (tnp c) (filter (meetsHardConstr (fpa c) (fm c) (tnt c)) s))) (filter (meetsHardConstr (fpa c) (fm c) (tnt c)) s)) mickey (zip (map (calcPenalty (mp c) (tnp c)) (filter (meetsHardConstr (fpa c) (fm c) (tnt c)) s)) (filter (meetsHardConstr (fpa c) (fm c) (tnt c)) s))


mickey :: [(Int, [Char])] -> Int
mickey list = min [x[0] | x <- list]

mini :: [(Int, [Char])] -> Int -> (Int, [Char])
mini sol min' = head (filter (==min') sol)
{-
zip (map (calcPenalty) (filter (meetsHardConstr (fpa c) (fm c) (tnt c) s))) (filter (meetsHardConstr (fpa c) (fm c) (tnt c) s))
-}



