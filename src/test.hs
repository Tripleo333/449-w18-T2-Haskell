import Text.Regex.Posix

verifyMachTaskParseError :: String -> Bool
verifyMachTaskParseError x = x =~ "^\\([0-9]+,[A-Z]\\)[ \t]*$" :: Bool

verifyMachTaskInvalid :: String -> Bool
verifyMachTaskInvalid x = x =~ "^\\([0-9],[A-H]\\)[ \t]*$" :: Bool

verifyTaskTaskParseError :: String -> Bool
verifyTaskTaskParseError x = x =~ "^\\([A-Z],[A-Z]\\)[ \t]*$" :: Bool

verifyTaskTaskInvalid :: String -> Bool
verifyTaskTaskInvalid x = x =~ "^\\([A-H],[A-H]\\)[ \t]*$" :: Bool

-- Expects any of these three things between 1 and 8 times:
--      Set of non-whitespace chars followed by a space
--      A space followed by a set of non-whitespace chars
--      A space followed by a set of non-whitespace chars followed by a space
-- Returns false if there are more than 8 sets of chars.
verifyMachPenaltiesParseError :: String -> Bool
verifyMachPenaltiesParseError x = x =~ "^(([^ |^\t]+ )|([^ |^\t]+)){1,8}[ \t]*$" :: Bool

-- Need to make it so that this doesn't accept stuff like a0.9bcddd0 (trailing and leading spaces, so on so forth)
-- Checks to see if the line contains a float or negative value
verifyMachPenaltiesInvalidPenalty :: String -> Bool
verifyMachPenaltiesInvalidPenalty x = x =~ "(([+-]?[0-9]*\\.[0-9]+)|([-][0-9]+))" :: Bool

--verifyTaskTaskPenaltyParseError
