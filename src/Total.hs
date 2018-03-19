module Total where

import Test
import Constraints

-- Input file -> For Loop Counter -> Current Line -> Constraints.ConstraintTup
total :: [String] -> Int -> Int -> Constraints.ConstraintTup -> Constraints.ConstraintTup

total x 0 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	|	verifyBlank (x !! y) = total (x) 0 (y + 1) z
	|	verifyName (x !! y) = total (x) 1 (y + 1) z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]

total x 1 y z = total (x) 300 (y + 1) z

total x 300 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyBlank (x !! y) = total (x) 2 (y + 1) z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]

total x 2 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyBlank (x !! y) = total (x) 2 (y + 1) z
  | verifyFPA (x !! y) = total (x) 3 (y + 1) z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]

total x 3 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyBlank (x !! y) = total (x) 301 (y) z
  | verifyMachTaskParseError (x !! y) = total (x) 4 y z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]

total x 4 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyMachTaskInvalid (x !! y) = total (x) 301 (y + 1) (Constraints.ConstraintTup ((fPA z)++[getMachTask (x !! y)]) (fM z) (tNt z) (mP z) (tNP z) [])
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["invalid machine/task"]

total x 301 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyBlank (x !! y) = total (x) 302 (y + 1) z
  | verifyMachTaskParseError (x !! y) = total (x) 4 y z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]

total x 302 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyBlank (x !! y) = total (x) 302 (y + 1) z
  | verifyMachTaskParseError (x !! y) = total (x) 4 y z
  | verifyFM (x !! y) = total (x) 5 (y + 1) z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]

total x 5 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyBlank (x !! y) = total (x) 303 (y) z
  | verifyMachTaskParseError (x !! y) = total (x) 6 y z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]

total x 6 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyMachTaskInvalid (x !! y) = total (x) 303 (y + 1) (Constraints.ConstraintTup (fPA z) ((fM z)++[getMachTask (x !! y)]) (tNt z) (mP z) (tNP z) [])
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["invalid machine/task"]

total x 303 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyBlank (x !! y) = total (x) 304 (y + 1) z
  | verifyMachTaskParseError (x !! y) = total (x) 6 y z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]

total x 304 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyBlank (x !! y) = total (x) 304 (y + 1) z
  | verifyMachTaskParseError (x !! y) = total (x) 6 y z
  | verifyTNT (x !! y) = total (x) 7 (y + 1) z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]

total x 7 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyBlank (x !! y) = total (x) 305 (y) z
  | verifyTaskTaskParseError (x !! y) = total (x) 8 y z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]

total x 8 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyTaskTaskInvalid (x !! y) = total (x) 305 (y + 1) (Constraints.ConstraintTup (fPA z) (fM z) ((tNt z)++[getTaskTask (x !! y)]) (mP z) (tNP z) [])
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["invalid machine/task"]

total x 305 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyBlank (x !! y) = total (x) 306 (y + 1) z
  | verifyMachTaskParseError (x !! y) = total (x) 8 y z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]

total x 306 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyBlank (x !! y) = total (x) 306 (y + 1) z
  | verifyMachTaskParseError (x !! y) = total (x) 8 y z
  | verifyMP (x !! y) = total (x) 9 (y + 1) z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]

-- Each set of three states is a copy and paste to ensure exactly 8 lines are read (excluding blank lines)
-- LINE 1
total x 9 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyBlank (x !! y) = total (x) 9 (y + 1) z
	| verifyMachPenaltiesPenaltyError (x !! y) = total (x) 10 y z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["machine penalty error"]

total x 10 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyMachPenaltiesInvalidPenalty (x !! y) = total (x) 11 (y + 1) (Constraints.ConstraintTup (fPA z) (fM z) (tNt z) ((mP z)++[getMachPenaltyLine (x !! y)]) (tNP z) [])
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["invalid penalty"]

-- LINE 2
total x 11 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyBlank (x !! y) = total (x) 11 (y + 1) z
	| verifyMachPenaltiesPenaltyError (x !! y) = total (x) 12 y z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["machine penalty error"]

total x 12 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyMachPenaltiesInvalidPenalty (x !! y) = total (x) 13 (y + 1) (Constraints.ConstraintTup (fPA z) (fM z) (tNt z) ((mP z)++[getMachPenaltyLine (x !! y)]) (tNP z) [])
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["invalid penalty"]

-- LINE 3
total x 13 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyBlank (x !! y) = total (x) 13 (y + 1) z
	| verifyMachPenaltiesPenaltyError (x !! y) = total (x) 14 y z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["machine penalty error"]

total x 14 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyMachPenaltiesInvalidPenalty (x !! y) = total (x) 15 (y + 1) (Constraints.ConstraintTup (fPA z) (fM z) (tNt z) ((mP z)++[getMachPenaltyLine (x !! y)]) (tNP z) [])
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["invalid penalty"]

-- LINE 4
total x 15 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyBlank (x !! y) = total (x) 15 (y + 1) z
	| verifyMachPenaltiesPenaltyError (x !! y) = total (x) 16 y z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["machine penalty error"]

total x 16 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyMachPenaltiesInvalidPenalty (x !! y) = total (x) 17 (y + 1) (Constraints.ConstraintTup (fPA z) (fM z) (tNt z) ((mP z)++[getMachPenaltyLine (x !! y)]) (tNP z) [])
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["invalid penalty"]

-- LINE 5
total x 17 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyBlank (x !! y) = total (x) 17 (y + 1) z
	| verifyMachPenaltiesPenaltyError (x !! y) = total (x) 18 y z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["machine penalty error"]

total x 18 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyMachPenaltiesInvalidPenalty (x !! y) = total (x) 19 (y + 1) (Constraints.ConstraintTup (fPA z) (fM z) (tNt z) ((mP z)++[getMachPenaltyLine (x !! y)]) (tNP z) [])
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["invalid penalty"]

-- LINE 6
total x 19 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyBlank (x !! y) = total (x) 19 (y + 1) z
	| verifyMachPenaltiesPenaltyError (x !! y) = total (x) 20 y z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["machine penalty error"]

total x 20 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyMachPenaltiesInvalidPenalty (x !! y) = total (x) 21 (y + 1) (Constraints.ConstraintTup (fPA z) (fM z) (tNt z) ((mP z)++[getMachPenaltyLine (x !! y)]) (tNP z) [])
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["invalid penalty"]

-- LINE 7
total x 21 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyBlank (x !! y) = total (x) 21 (y + 1) z
	| verifyMachPenaltiesPenaltyError (x !! y) = total (x) 22 y z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["machine penalty error"]

total x 22 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyMachPenaltiesInvalidPenalty (x !! y) = total (x) 23 (y + 1) (Constraints.ConstraintTup (fPA z) (fM z) (tNt z) ((mP z)++[getMachPenaltyLine (x !! y)]) (tNP z) [])
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["invalid penalty"]

-- LINE 8
total x 23 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyBlank (x !! y) = total (x) 23 (y + 1) z
	| verifyMachPenaltiesPenaltyError (x !! y) = total (x) 24 y z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["machine penalty error"]

total x 24 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyMachPenaltiesInvalidPenalty (x !! y) = total (x) 25 (y + 1) (Constraints.ConstraintTup (fPA z) (fM z) (tNt z) ((mP z)++[getMachPenaltyLine (x !! y)]) (tNP z) [])
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["invalid penalty"]

total x 25 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	|	verifyBlank (x !! y) = total (x) 25 (y + 1) z
  | verifyTNP (x !! y) = total (x) 26 (y + 1) z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["machine penalty error"]

total x 26 y z
--	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| y >= (length x) = z
	| verifyBlank (x !! y) = total (x) 26 (y + 1) z
  | verifyTooNearPenaltiesParseError (x !! y) = total (x) 27 (y) z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]

total x 27 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyTooNearPenaltiesInvalidTask (x !! y) = total (x) 28 (y) z
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["invalid task"]

total x 28 y z
	|	y >= (length x) = Constraints.ConstraintTup [] [] [] [[]] [] ["Error while parsing input file"]
	| verifyTooNearPenaltiesInvalidPenalty (x !! y) = total (x) 26 (y + 1) (Constraints.ConstraintTup (fPA z) (fM z) (tNt z) (mP z) ((tNP z)++[getTaskTaskPenalty (x !! y)]) [])
  |	otherwise = Constraints.ConstraintTup [] [] [] [[]] [] ["invalid penalty"]