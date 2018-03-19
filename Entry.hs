module Entry where
import Verify
import Constraints

data Fallow a = Fallow{ output :: String
					, line	 :: Int}
data Loop8 = Loop8{ count	:: Int
					, out :: String
					, lin	 :: Int}
loop8 :: (Int, String, Int)->Loop8
loop8 (m,o,p) = Loop8{ 	count   = m
					,	out		= o
					,	lin		= p}	

fallow :: (String,Int)->Fallow
fallow (str,c) = Fallow{ output = str ++ "?"
						,line	= c } 
					
checkverifyName :: ([String],Int)->Fallow
checkverifyName = do 
					[x,c] <-getargs
					if verifyName(head x) == True
						then do
							let a = fallow("",c)
							return a
						else do
							let a = fallow("Error while parsing input file",c)
							return a
checkverifyBlank :: ([String],Int)->Fallow
checkverifyBlank = do 
					[x,c] <-getargs
					if verifyBlank(x!!c) == True
						then do
							let a = fallow("",c+1)
							return a
						else do
							let a = fallow("Error while parsing input file",c)
							return a

checkverifyFPa :: ([String],Int)->Fallow
checkverifyFPa = do
					[x,c] <-getargs
					if verifyFPa (x !! c) == True --check forced partial assign
						then do
							let a = fallow("",c+1)
							return a
						else do
							let a = fallow("Error while parsing input file",c)
							return a
loopFPa :: ([String],Fallow)->Fallow
loopFPa = do
			[x,a] <-getargs
			if verifyMachTaskParseError(x !! a line) == True
				then do
					if verifyMachTaskInvalid(x !! a line) == True
						then do
							ConstraintTup fPa (x !! a line)
							let a = fallow("",a line + 1)
							let a = loopFPa (x,a)
							return a
						else do
							let a = fallow("invalid machine/task",a line)
							return a
				else do
					if verifyBlank (x !! a line) == True	-- check if end of FPa
						then do
							let a = fallow("",a line + 1)
							return a
						else do
							let a = fallow("Error while parsing input file",a line)
							return a
checkverifyFM :: ([String],Int)->Fallow
checkverifyFM = do
					[x,c] <-getargs
					if verifyFM (x !! c) == True --check forced partial assign
						then do
							let a = fallow("",c+1)
							return a
						else do
							let a = fallow("Error while parsing input file",c)
							return a
loopFM :: ([String],Fallow)->Fallow
loopFM = do
			[x,a] <-getargs
			if verifyMachTaskParseError(x !! a line) == True
				then do
					if verifyMachTaskInvalid(x !! a line) == True
						then do
							ConstraintTup fM (x !! a line)
							let a = fallow("",a line + 1)
							let a = loopFM (x,a)
							return a
						else do
							let a = fallow("invalid machine/task",a line)
							return a
				else do
					if verifyBlank (x !! a line) == True	-- check if end of FM
						then do
							let a = fallow("",a line + 1)
							return a
						else do
							let a = fallow("Error while parsing input file",a line)
							return a
checkverifyTNT :: ([String],Int)->Fallow
checkverifyTNT = do
					[x,c] <-getargs
					if verifyTNT (x !! c) == True --check forced partial assign
						then do
							let a = fallow("",c+1)
							return a
						else do
							let a = fallow("Error while parsing input file",c)
							return a
loopTNT :: ([String],Fallow)->Fallow								
loopTNT = do
			[x,a] <-getargs
			if verifyTaskTaskParseError(x !! a line) == True
				then do
					if verifyTaskTaskInvalid(x !! a line) == True
						then do
							ConstraintTup tNt (x !! a line)
							let a = fallow("",a line + 1)
							let a = loopFM (x,a)
							return a
						else do
							let a = fallow("invalid machine/task",a line)
							return a
				else do
					if verifyBlank (x !! a line) == True	-- check if end of FM
						then do
							let a = fallow("",a line + 1)
							return a
						else do
							let a = fallow("Error while parsing input file",a line)
							return a
checkverifyMP :: ([String],Int)->Fallow
checkverifyMP = do
					[x,c] <-getargs
					if verifyMP (x !! c) == True --check forced partial assign
						then do
							let a = fallow("",c+1)
							return a
						else do
							let a = fallow("Error while parsing input file",c)
							return a
loopMP :: ([String],Fallow,Int)->Loop8
loopMP = do 
			[x,a,n] <-getargs
			if n == 0
				then do
					if verifyBlank (x !! a line) == True	-- check if end of FM
						then do
							let a = fallow("",a line+1)
							let b = loop8(n,a output,a line)
							return b
						else do
							let a = fallow("",a line)
							let b = loop8(n,a output,a line)
							return b
				else do
					if verifyMachPenaltiesParseError (x !! a line) == True
						then do
							if verifyMachPenaltiesPenaltyError (x !! a line) == True
								then do
									if verifyMachPenaltiesInvalidPenalty (x !! a line) == True
										then do
											ConstraintTup mP (getMachPenaltyLine(x !! a line))
											let a = fallow("",a line+1)
											let b = loop8(n-1,a output,a line)
											let b = loopMP (x,a,b count)
											return b
										else do
											let a = fallow("invalid penalty",a line)
											let b = loop8(n,a output,a line)
											return b
								else do
									let a = fallow("machine penalty error",a line)
									let b = loop8(n,a output,a line)
									return b
								
						else do
							if verifyBlank (x !! c) == True
								then do
									let a = fallow("machine penalty error",a line)
									let b = loop8(n,a output,a line)
									return b
								
								else do
									let a = fallow("Error while parsing input file",a line)
									let b = loop8(n,a output,a line)
									return b
checkverifyTNP :: ([String],Int)->Fallow
checkverifyTNP = do
					[x,c] <-getargs
					if verifyTNP (x !! c) == True --check forced partial assign
						then do
							let a = fallow("",c+1)
							return a
						else do
							let a = fallow("Error while parsing input file",c)
							return a

loopTNP :: ([String],Int) -> ([String],Int)		--similar to previous loops but with TNP
loopTNP = do
		[x,a] <-getargs
		if verifyTooNearPenaltiesParseError(x !! a line) == True
			then do
				if verifyTooNearPenaltiesInvalidTask (x !! a line) == True
					then do
						if verifyTooNearPenaltiesInvalidPenalty (x !! a line) == True
							then do
								ConstraintTup fM (getTaskTaskPenalty(x !! a line))
								let a = fallow("",a line + 1)
								let a = loopFM (x,a)
								return a
							else do
								let a = fallow("invalid penalty",a line)
								return a
					else do
						let a = fallow("invalid task",a line)
						return a
				else do
					if verifyBlank (x !! a line) == True	-- check if end of FM
						then do
							let a = fallow("",a line + 1)
							return a
						else do
							let a = fallow("Error while parsing input file",a line)
							return a						
entry :: [String]->String
entry = do
		[x] <-getargs
		let c = 0	--counter to track line of list
		let a = fallow("",c)
		let a = checkverifyName(x,a line)
		let a = checkverifyBlank(x,a line)
		let a = checkverifyFPa(x,a line)
		let a = loopFPa(x,a)
		let a = checkverifyFM(x,a line)
		let a = loopFM(x,a)
		let a = checkverifyTNT(x,a line)
		let a = loopTNT(x,a)
		let a = checkverifyMP(x,a line)
		let b = loopMP(x,a,8)
		let a = checkverifyTNP(x,b fall line)
		let a = loopTNP(x,a)
		x+7	