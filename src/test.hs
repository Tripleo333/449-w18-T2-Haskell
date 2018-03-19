module Test where

import Text.Regex.Posix
import Data.Char



verifyName :: String -> Bool
verifyName x = x =~ "^Name:[ \t]*$" :: Bool

verifyFPA :: String -> Bool
verifyFPA x = x =~ "^forced partial assignment:[ \t]*$" :: Bool

verifyFM :: String -> Bool
verifyFM x = x =~ "^forbidden machine:[ \t]*$" :: Bool

verifyTNT :: String -> Bool
verifyTNT x = x =~ "^too-near tasks:[ \t]*$" :: Bool

verifyMP :: String -> Bool
verifyMP x = x =~ "^machine penalties:[ \t]*$" :: Bool

verifyTNP :: String -> Bool
verifyTNP x = x =~ "^too-near penalities[ \t]*$" :: Bool

verifyBlank :: String -> Bool
verifyBlank x = x =~ "^[ \t]*$" :: Bool

verifyMachTaskParseError :: String -> Bool
verifyMachTaskParseError x = x =~ "^\\([0-9]+,[A-Z]\\)[ \t]*$" :: Bool

verifyMachTaskInvalid :: String -> Bool
verifyMachTaskInvalid x = x =~ "^\\([1-8],[A-H]\\)[ \t]*$" :: Bool

getMachTask :: String -> (Int, Char)
getMachTask x = (digitToInt (x !! 1), x !! 3)

verifyTaskTaskParseError :: String -> Bool
verifyTaskTaskParseError x = x =~ "^\\([A-Z],[A-Z]\\)[ \t]*$" :: Bool

verifyTaskTaskInvalid :: String -> Bool
verifyTaskTaskInvalid x = x =~ "^\\([A-H],[A-H]\\)[ \t]*$" :: Bool

getTaskTask :: String -> (Char, Char)
getTaskTask x = (x !! 1, x !! 3)

-- Expects any of these three things between 1 and 8 times:
--      Set of non-whitespace chars followed by a space
--      A space followed by a set of non-whitespace chars
--      A space followed by a set of non-whitespace chars followed by a space
-- Returns false if there are more than 8 sets of chars.
verifyMachPenaltiesParseError :: String -> Bool
verifyMachPenaltiesParseError x = x =~ "^(([^ |^\t]+ )|([^ |^\t]+)){1,8}[ \t]*$" :: Bool

-- Returns false if there is less than 8 seperate things (don't have to be numbers)
-- DOES NOT (and cannot) check if there is more or less than 8 lines (This needs to be done in the caller)
verifyMachPenaltiesPenaltyError :: String -> Bool
verifyMachPenaltiesPenaltyError x = x =~ "^([^ |^\t]+ ){7}([^ |^\t]+){1}[ \t]*$" :: Bool

-- Makes sure the line is 8 natural numbers seperated by spaces then followed by any amount of white space (spaces and tabs)
verifyMachPenaltiesInvalidPenalty :: String -> Bool
verifyMachPenaltiesInvalidPenalty x = x =~ "^([0-9]+ ){7}([0-9]+){1}[ \t]*$" :: Bool
-- verifyMachPenaltiesInvalidPenalty x = x =~ "(([+-]?[0-9]*\\.[0-9]+)|([-][0-9]+))" :: Bool

getMachPenaltyLine :: String -> [Int]
getMachPenaltyLine x = map read (words x)

verifyTooNearPenaltiesParseError :: String -> Bool
verifyTooNearPenaltiesParseError x = x =~ "^\\([A-Z],[A-Z],(.+)\\)[ \t]*$" :: Bool

verifyTooNearPenaltiesInvalidTask :: String -> Bool
verifyTooNearPenaltiesInvalidTask x = x=~ "^\\([A-H],[A-H],(.+)\\)[ \t]*$"

verifyTooNearPenaltiesInvalidPenalty :: String -> Bool
verifyTooNearPenaltiesInvalidPenalty x = x=~ "^\\([A-H],[A-H],[0-9]+\\)[ \t]*$"

getPenalty :: (String, String) -> String
getPenalty (x:xs, z)
    | isDigit x = getPenalty (xs, (z ++ [x]))
    | otherwise = z

goToPenalty :: String -> Int
goToPenalty (x:xs)
    | isDigit x = read (getPenalty (x:xs, ""))::Int
    | otherwise = goToPenalty xs

getTaskTaskPenalty :: String -> (Char, Char, Int)
getTaskTaskPenalty x = (x !! 1, x !! 3, goToPenalty x)



--verifyTaskTaskPenaltyParseError
