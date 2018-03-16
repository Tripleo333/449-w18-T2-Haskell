removeLetter :: [Char] -> Char -> [Char]
removeLetter x y = [a | a <- x, a /= y]

-- State -> Length -> Constraints -> Current Global Minimum -> Chars not yet used in State -> Final State
branch :: [Char] -> Int -> Constraints -> Int -> [Char] -> [Char]
branch w [x | x >= 8] y z a
branch w x y z a
    -- If the penalty value of state is >= current global minimum, return char list containing only '0'
    | (GET PENALTY VALUE OF W) >= z = ['0']
    | otherwise = getReturn [branch (w ++ b) x y z (removeLetter a, b) | b <- a]

getReturn :: [[Char]] -> Constraints -> [Char]
getReturn x y = [z | z <- x, GET PENALTY VALUE OF z < ]
