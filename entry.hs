
entry :: [String]->String
entry = do
	[x] <-getArgs
	let c = 0	--counter to track line of list
    if verifyName (head x) == True
		then --send name to somewhere
			c = c + 1 -- update counter
	else
		then 
			let y = "Error while parsing input file" :: string -- return string for writing to file
	if verifyBlank (x !! c) == True	--check for blank space
		then 
			c = c + 1
	else
		then
			let y = "Error while parsing input file" :: string -- return string for writing to file
	if verifyFPA (x !! c) == True --check forced partial assign
		then
			c = c + 1
	else
		then
			let y = "Error while parsing input file" :: string -- return string for writing to file
	loopFPA :: ([String],Int) -> ([String],Int)	--attempt a creating a recursive function to act as loop
		if verifyMachTaskParseError (x !! c) == True	
			then
				if verifyMachTaskInvalid (x !! c) == True
					then
						c = c + 1		--update counter
						ConstraintTup fPA x	--add line to fPA list
						loopFPA (x,c)	--recursivly loop with new counter and line
						(x,c) :: ([String],Int)	--return with new line counter might need to update idk
				else
					then
						let y = "invalid machine/task" :: string -- return string for writing to file
		else
			then
				if verifyBlank (x !! c) == True	-- check if end of FPA
					then
						c = c + 1
						(x,c) :: ([String],Int) --return with new line counter
				else
					then
						let y = "Error while parsing input file" :: string -- return string for writing to file if not blank and not valid FPA
	if verifyFM (x !! c) == True
		then
			c = c + 1
	else
		then
			let y = "Error while parsing input file" :: string -- return string for writing to file		
	loopFM :: ([String],Int) -> ([String],Int)    --same as above but with FM
		if verifyMachTaskParseError (x !! c) == True
			then
				if verifyMachTaskInvalid (x !! c) == True
					then
						c = c + 1
						ConstraintTup fM x
						loopFM (x,c)
						(x,c) :: ([String],Int)
				else
					then
						let y = "invalid machine/task" :: string -- return string for writing to file
		else
			then
				if verifyBlank (x !! c) == True
					then
						c = c + 1
						(x,c) :: ([String],Int)
				else
					then
						let y = "Error while parsing input file" :: string -- return string for writing to file	
	if verifyTNT (x !! c) == True
		then
			c = c + 1
	else
		then
			let y = "Error while parsing input file" :: string -- return string for writing to file
	loopTNT :: ([String],Int) -> ([String],Int) --same as above but with TNT
		if verifyTaskTaskParseError (x !! c) == True
			then
				if verifyTaskTaskInvalid (x !! c) == True
					then
						c = c + 1
						ConstraintTup tNt x
						loopTNT (x,c)
						(x,c) :: ([String],Int)
				else
					then
						let y = "invalid machine/task" :: string -- return string for writing to file
		else
			then
				if verifyBlank (x !! c) == True
					then
						c = c + 1
						(x,c) :: ([String],Int)
				else
					then
						let y = "Error while parsing input file" :: string -- return string for writing to file
	if verifyMP (x !! c) == True
		then
			c = c + 1
	else
		then
			let y = "Error while parsing input file" :: string -- return string for writing to file
			
	loopMP 8	--different from above loops trying to make this one work exactly 8 times any less and it fails any mroe and it fails

	loopMP :: ([String],Int,Int) -> ([String],Int,Int)
	loopMP 0 = return
	loopMP n = 
		do
			if verifyMachPenaltiesParseError (x !! c) == True
				then
					if verifyMachPenaltiesPenaltyError (x !! c) == True
						then
							if verifyMachPenaltiesInvalidPenalty (x !! c) == True
								then
									c = c + 1
									ConstraintTup mP (getMachPenaltyLine x !! c) --tries to call function to fix MP so it can be inputyed to constraints
									loopMP (x,c,n-1)
									(x,c,n) :: ([String],Int)
							else
								then
									let y = "invalid penalty" :: string -- return string for writing to file
									
					else
						then
							let y = "machine penalty error" :: string -- return string for writing to file
			else
				then
					if verifyBlank (x !! c) == True
						then
							let y = "machine penalty error" :: string -- return string for writing to file
					else
						then
							let y = "Error while parsing input file" :: string -- return string for writing to file
	if verifyTNP (x !! c) == True
		then
			c = c + 1
	else
		then
			let y = "Error while parsing input file" :: string -- return string for writing to file
	loopTNP :: ([String],Int) -> ([String],Int)		--similar to previous loops but with TNP
		if verifyTooNearPenaltiesParseError (x !! c) == True
			then
				if verifyTooNearPenaltiesInvalidTask (x !! c) == True
					then
						if verifyTooNearPenaltiesInvalidPenalty (x !! c) == True
							then
								c = c + 1
								ConstraintTup tNP (getTaskTaskPenalty x !! c) --tries to call function to fix TNP so it can be inputyed to constraints
								loopTNP (x,c)
								(x,c) :: ([String],Int)
						else
							then
								let y = "invalid penalty" :: string -- return string for writing to file
				else
					then
						let y = "invalid task" :: string -- return string for writing to file
		else
			then
				if verifyBlank (x !! c) == True
					then
						c = c + 1
						(x,c) :: ([String],Int)
				else
					then
						let y = "Error while parsing input file" :: string -- return string for writing to file
			