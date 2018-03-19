module Entry where
import Verify
import Constraints

data Fallow = Fallow{ output :: String
					, li	 :: Int}
data Loop8 = Loop8{ count	:: Int
					, out :: String
					, lin	 :: Int}
loop8 :: (Int, String, Int)->Loop8
loop8 (m,o,p) = Loop8{ 	count   = m
					,	out		= o
					,	lin		= p}	

fallow :: (String,Int)->Fallow
fallow (str,c) = Fallow{ output = str ++ "?"
						,li	= c } 
					
checkverifyName :: ([String],Int)->Fallow
checkverifyName (x,c)= do 
					
					if verifyName(head x) == True
						then do
							let a = fallow("",c)
							return a
						else do
							let a = fallow("Error while parsing input file",c)
							return a
checkverifyBlank :: ([String],Int)->Fallow
checkverifyBlank (x,c)= do 
					
					if verifyBlank(x!!c) == True
						then do
							let a = fallow("",c+1)
							return a
						else do
							let a = fallow("Error while parsing input file",c)
							return a

checkverifyFPa :: ([String],Int)->Fallow
checkverifyFPa (x,c)= do
					
					if verifyFPa (x !! c) == True --check forced partial assign
						then do
							let a = fallow("",c+1)
							return a
						else do
							let a = fallow("Error while parsing input file",c)
							return a
loopFPa :: ([String],Fallow)->Fallow
loopFPa (x,a)= do
			
			if verifyMachTaskParseError(x !! li a) == True
				then do
					if verifyMachTaskInvalid(x !! li a) == True
						then do
							ConstraintTup fPa (getMachTask(x !! li a))
							let a = fallow("",li a + 1)
							let a = loopFPa (x,a)
							return a
						else do
							let a = fallow("invalid machine/task",li a)
							return a
				else do
					if verifyBlank (x !! li a) == True	-- check if end of FPa
						then do
							let a = fallow("",li a + 1)
							return a
						else do
							let a = fallow("Error while parsing input file",li a)
							return a
checkverifyFM :: ([String],Int)->Fallow
checkverifyFM (x,c)= do
					
					if verifyFM (x !! c) == True --check forced partial assign
						then do
							let a = fallow("",c+1)
							return a
						else do
							let a = fallow("Error while parsing input file",c)
							return a
loopFM :: ([String],Fallow)->Fallow
loopFM (x,a)= do
			
			if verifyMachTaskParseError(x !! li a) == True
				then do
					if verifyMachTaskInvalid(x !! li a) == True
						then do
							ConstraintTup fM (getMachTask(x !! li a))
							let a = fallow("",li a + 1)
							let a = loopFM (x,a)
							return a
						else do
							let a = fallow("invalid machine/task",li a)
							return a
				else do
					if verifyBlank (x !! li a) == True	-- check if end of FM
						then do
							let a = fallow("",li a + 1)
							return a
						else do
							let a = fallow("Error while parsing input file",li a)
							return a
checkverifyTNT :: ([String],Int)->Fallow
checkverifyTNT (x,c)= do
					
					if verifyTNT (x !! c) == True --check forced partial assign
						then do
							let a = fallow("",c+1)
							return a
						else do
							let a = fallow("Error while parsing input file",c)
							return a
loopTNT :: ([String],Fallow)->Fallow								
loopTNT (x,a)= do
			
			if verifyTaskTaskParseError(x !! li a) == True
				then do
					if verifyTaskTaskInvalid(x !! li a) == True
						then do
							ConstraintTup tNt (getTaskTask(x !! li a))
							let a = fallow("",li a + 1)
							let a = loopFM (x,a)
							return a
						else do
							let a = fallow("invalid machine/task",li a)
							return a
				else do
					if verifyBlank (x !! li a) == True	-- check if end of FM
						then do
							let a = fallow("",li a + 1)
							return a
						else do
							let a = fallow("Error while parsing input file",li a)
							return a
checkverifyMP :: ([String],Int)->Fallow
checkverifyMP (x,c)= do
					
					if verifyMP (x !! c) == True --check forced partial assign
						then do
							let a = fallow("",c+1)
							return a
						else do
							let a = fallow("Error while parsing input file",c)
							return a
loopMP :: ([String],Fallow,Int)->Loop8
loopMP (x,a,n)= do 
			if n == 0
				then do
					if verifyBlank (x !! li a) == True	-- check if end of FM
						then do
							let a = fallow("",li a+1)
							let b = loop8(n,output a,li a)
							return b
						else do
							let a = fallow("",li a)
							let b = loop8(n,output a,li a)
							return b
				else do
					if verifyMachPenaltiesParseError (x !! li a) == True
						then do
							if verifyMachPenaltiesPenaltyError (x !! li a) == True
								then do
									if verifyMachPenaltiesInvalidPenalty (x !! li a) == True
										then do
											ConstraintTup mP (getMachPenaltyLine(x !! li a))
											let a = fallow("",li a+1)
											let b = loop8(n-1,output a,li a)
											let b = loopMP (x,a,count b)
											return b
										else do
											let a = fallow("invalid penalty",li a)
											let b = loop8(n,output a,li a)
											return b
								else do
									let a = fallow("machine penalty error",li a)
									let b = loop8(n,output a,li a)
									return b
								
						else do
							if verifyBlank (x !! li a) == True
								then do
									let a = fallow("machine penalty error",li a)
									let b = loop8(n,output a,li a)
									return b
								
								else do
									let a = a.("Error while parsing input file",li a)
									let b = loop8(n,output a,li a)
									return b
checkverifyTNP :: ([String],Int)->Fallow
checkverifyTNP (x,c)= do
					
					if verifyTNP (x !! c) == True --check forced partial assign
						then do
							let a = fallow("",c+1)
							return a
						else do
							let a = fallow("Error while parsing input file",c)
							return a

loopTNP :: ([String],Fallow) -> Fallow		--similar to previous loops but with TNP
loopTNP (x,a)= do
			let m = (length li)-1
			let q = m+1
			if verifyTooNearPenaltiesParseError(x !! m) == True
				then do
					if verifyTooNearPenaltiesInvalidTask (x !! m) == True
						then do
							if verifyTooNearPenaltiesInvalidPenalty (x !! m) == True
								then do
									--ConstraintTup fM (getTaskTaskPenalty(x !! m)
									let a1 = fallow("",q)
									let a2 = loopFM (x,a1)
									return a2
								else do
									let a1 = fallow("invalid penalty",m)
									return a1
						else do
							let a1 = fallow("invalid task",m)
							return a1
					else do
					
						if verifyBlank (x !! m) == True	-- check if end of FM
							then do
								let a1 = fallow("",m)
								return a1
							else do
								let a = a.("Error while parsing input file",m)
								return a						
entry :: [String]->String
entry x = do
		let c = 0	--counter to track line of list
		let a1 = [].fallow("",c)
		let a2 = checkverifyName(x,li a1)
		let a3 = checkverifyBlank(x,li a2)
		let a4 = checkverifyFPa(x,li a3)
		let a5 = loopFPa(x,a4)
		let a6 = checkverifyFM(x,li a5)
		let a7 = loopFM(x,a6)
		let a8 = checkverifyTNT(x,li a7)
		let a9 = loopTNT(x,a8)
		let a10 = checkverifyMP(x,li a9)
		let b = loopMP(x,a10,8)
		let a11 = checkverifyTNP(x,lin b)
		let a12 = loopTNP(x,a11)
		let d =	output a12
		return d