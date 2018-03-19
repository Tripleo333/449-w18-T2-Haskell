module Entry where
import test
import Constraints

data Fallow = Fallow{ output :: String
					, li	 :: Int}
data Loop8 = Loop8{ count	:: Int
					, out :: String
					, lin	 :: Int}
data Pair = P String Int

					
		
loop8 :: (Int, String, Int)->Loop8
loop8 (m,o,p) = Loop8{ 	count   = m
					,	out		= o
					,	lin		= p}	

fallow :: (String,Int)->Fallow
fallow (str,c) = Fallow{ output = str ++ "?"
						,li	= c } 

addOne :: Int->Int
addOne x = 
		let
			r = 1
			h = x
			x = h+r
		in x
		
checkverifyName :: ([String],Int)->(String,Int)
checkverifyName (x,c)= do 
					
					if verifyName(head x) == True
						then 	let
									(i,j) = ("",(succ j))
								in(i,j)
						else	let
									(i,j) = ("Error while parsing input file",c)
								in(i,j)
checkverifyBlank :: ([String],Int)->(String,Int)
checkverifyBlank (x,c)= do 
					
					if verifyBlank(x!!c) == True
						then 	let
									(i,j) = ("",c+1)
								in (i,j)
						else 	let
									(i,j) = ("Error while parsing input file",c)
								in(i,j)

checkverifyFPa :: ([String],Int)->(String,Int)
checkverifyFPa (x,c)= do
					
					if verifyFPA (x !! c) == True --check forced partial assign
						then 	let
									(i,j) = ("",c+1)
								in(i,j)
						else 	let
									(i,j) = ("Error while parsing input file",c)
								in(i,j)
loopFPa :: ([String],String,Int)->(String,Int)
loopFPa (x,i,j)= do
			
			if verifyMachTaskParseError(x !! j) == True
				then do
					if verifyMachTaskInvalid(x !! j) == True
						then 	let
									--ConstraintTup fPa (getMachTask(x !! j))
									(i,j) = loopFPa (x,"",j+1)
								in(i,j)
						else 	let
									(i,j) = ("invalid machine/task",j)
								in(i,j)
				else do
					if verifyBlank (x !! j) == True	-- check if end of FPa
						then 	let
									(i,j) = ("",j+1)
								in (i,j)
						else 	let
									(i,j) = ("Error while parsing input file",j)
								in (i,j)
checkverifyFM :: ([String],Int)->(String,Int)
checkverifyFM (x,c)= do
					
					if verifyFM (x !! c) == True --check forced partial assign
						then 	let
									(i,j) = ("",c+1)
								in (i,j)
						else	let
									(i,j) = ("Error while parsing input file",c)
								in (i,j)
loopFM :: ([String],String,Int)->(String,Int)
loopFM (x,i,j)= do
			
			if verifyMachTaskParseError(x !! j) == True
				then do
					if verifyMachTaskInvalid(x !! j) == True
						then 	let
									--ConstraintTup fM (getMachTask(x !! j))
									(i,j) = loopFM (x,"",j+1)
								in (i,j)
						else 	let
									(i,j) = ("invalid machine/task",j)
								in (i,j)
				else do
					if verifyBlank (x !! j) == True	-- check if end of FM
						then 	let
									(i,j) = ("",j+1)
								in (i,j)
						else 	let
									(i,j) = ("Error while parsing input file",j)
								in (i,j)
checkverifyTNT :: ([String],Int)->(String,Int)
checkverifyTNT (x,c)= do
					
					if verifyTNT (x !! c) == True --check forced partial assign
						then 	let
									(i,j) = ("",c+1)
								in (i,j)
						else 	let
									(i,j) = ("Error while parsing input file",c)
								in (i,j)
loopTNT :: ([String],String,Int)->(String,Int)								
loopTNT (x,i,j)= do
			
			if verifyTaskTaskParseError(x !! j) == True
				then do
					if verifyTaskTaskInvalid(x !! j) == True
						then 	let
									--ConstraintTup tNt (getTaskTask(x !! j))
									(i,j) = loopFM (x,"",j+1)
								in (i,j)
						else	let
									(i,j) = ("invalid machine/task",j)
								in (i,j)
				else do
					if verifyBlank (x !! j) == True	-- check if end of FM
						then 	let
									(i,j) = ("",j+1)
								in (i,j)
						else	let
									(i,j) = ("Error while parsing input file",j)
								in (i,j)
checkverifyMP :: ([String],Int)->(String,Int)
checkverifyMP (x,c)= do
					
					if verifyMP (x !! c) == True --check forced partial assign
						then 	let
									(i,j) = ("",c+1)
								in (i,j)
						else 	let
									(i,j) = ("Error while parsing input file",c)
								in (i,j)
loopMP :: ([String],String,Int,Int)->(String,Int,Int)
loopMP (x,i,j,n)= do 
			if n == 0
				then do
					if verifyBlank (x !! j) == True	-- check if end of FM
						then 	let
									(i,j) = ("",j+1)
								in (i,j,n)
						else 	let
									(i,j) = ("machine penalty error",j)
								in (i,j,n)
				else do
					if verifyMachPenaltiesParseError (x !! j) == True
						then do
							if verifyMachPenaltiesPenaltyError (x !! j) == True
								then do
									if verifyMachPenaltiesInvalidPenalty (x !! j) == True
										then 	let
													--ConstraintTup mP (getMachPenaltyLine(x !! li a))
													(i,j,n) = loopMP (x,"",j+1,n-1)
												in (i,j,n)
										else 	let
													(i,j,n) = ("invalid penalty",j,n-1)
												in(i,j,n)
								else	let
											(i,j,n) = ("machine penalty error",j,n-1)
										in(i,j,n)
								
						else do
							if verifyBlank (x !! j) == True
								then 	let
											(i,j,n) = ("machine penalty error",j,n-1)
										in(i,j,n)
								
								else 	let
											(i,j,n) = ("Error while parsing input file",j,n-1)
										in (i,j,n)
checkverifyTNP :: ([String],Int)->(String,Int)
checkverifyTNP (x,c)= do
					
					if verifyTNP (x !! c) == True --check forced partial assign
						then 	let
									(i,j) = ("",(c+1))
								in (i,j)
						else 	let
									(i,j) = ("Error while parsing input file",c)
								in (i,j)

loopTNP :: ([String],String,Int) -> (String,Int)		--similar to previous loops but with TNP
loopTNP (x,i,j)= do
			if verifyTooNearPenaltiesParseError(x !! j) == True
				then do
					if verifyTooNearPenaltiesInvalidTask (x !! j) == True
						then do
							if verifyTooNearPenaltiesInvalidPenalty (x !! j) == True
								then 	let
											--ConstraintTup fM (getTaskTaskPenalty(x !! m)
											(i,j) = loopFM (x,"",j+1)
										in (i,j)
								else 	let
											(i,j) = ("invalid penalty",j)
										in (i,j)
						else 	let
									(i,j) = ("invalid task",j)
								in (i,j)
					else do
					
						if verifyBlank (x !! j) == True	-- check if end of FM
							then 	let
										(i,j) = ("",j)
									in (i,j)
							else 	let
										(i,j) = ("Error while parsing input file",j)
									in (i,j)						
entry :: [String]->String
entry x = do 
		let	(i,j) = ("",0)
		let	(i,j) = checkverifyName(x,j)
		let	(i,j) = checkverifyBlank(x,j)
		let	(i,j) = checkverifyFPa(x,j)
		let	(i,j) = loopFPa(x,i,j)
		let	(i,j) = checkverifyFM(x,j)
		let	(i,j) = loopFM(x,i,j)
		let	(i,j) = checkverifyTNT(x,j)
		let	(i,j) = loopTNT(x,i,j)
		let	(i,j) = checkverifyMP(x,j)
		let	(i,j,n) = loopMP(x,i,j,7)
		let	(i,j) = checkverifyTNP(x,j)
		let	(i,j) = loopTNP(x,i,j)
		return i