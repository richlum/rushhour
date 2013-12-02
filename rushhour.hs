-- 20131021
-- richard lum

-- will work for arbitrary sized rectangular matrix but must have min size 2 
-- row car XX to be in row 2 (zero based).  Target presumed to be furthest right
-- point in row 2.

-- This version uses heurestic to get next 'best' state to explore

-- input sample
-- rushhour ["--B---","--B---","XXB---","--AA--","------","------"]
-- rush_print ["--B---","--B---","XXB---","--AA--","------","------"]
-- rushhour ["--B---","--B---","XX----","--AA--","------","------"]
-- rushhour ["--B---","--B-DD","XX----","--AA--","ZZYY--","QQ--PP"]
-- rushhour 
-- ["-AB-C-"
-- ,"-AB-CD"
-- ,"XXB-CD-"
-- ,"Z----E"
-- ,"ZQ---E"
-- ,"ZQ-PPP"]
-- rushhour ["-AB---","-AB-CD","XXB-CD-","Z---AA","ZQ----","ZQ----"]
-- rush_print ["------","--B---","XXB--c","--AA-c","-----c","-ddddd"]    12 step solve


module RushHour
(		
	moveRCar,moveLCar,moveCarLeftInMaze,moveCarRightInMaze,
	rushhour,			rush_print,moveAllCarsLeft, moveAllCarsRight,
	isGoalState,		statesearch,rateMaze,mazeRank,
	generateNewStates,  getCarList,isSameLength,
	cpos			,	rpos, moveCarDown,moveCarUp,
	moveAllCarsUp ,moveAllCarsDown 

)	where
import Debug.Trace (trace)
import Data.List

-- this is our goal state constant  --  not needed but for visual reference
-- isGoalState does hard coded check for position. 
goal = ["------","------","----XX","------","------","------"]
emptyrow = "------"
rows = 6
cols = 6

---------- entry points
rushhour::[String] ->  [[String]]
rushhour start 
	| not (isSameLength start)		= error "bad matrix"
	| (rpos 'X' start) /= 2			= error "X not in row 2"
	| (rowCarSize 'X' (start!!2))<2	= error "X must be a row cars of at least size 2"
rushhour start = trace ("\nstates explored : " ++ (show (length (snd result))) ++ 
						"\nsolution length : " ++ (show (length (fst result))))  $
				(reverse (filter (not.null) (fst result )))  
				where
					result = (statesearch [start]  [[]] [[]]        optlist); 
--                              startmaze  target visted sucesspath  listOfMovesToExplore-partialfunction  
					optlist = [moveAllCarsLeft ,moveAllCarsRight,
								moveAllCarsUp ,moveAllCarsDown ]

--  visited required so we can compare maze state to make sure we dont
--  cycle since vehicles can and may have to move back and forth

-- from rodrigo alves posting to connect
rush_print::[String]  -> IO()
rush_print start   = printStrMatrix (rushhour start   )
---------- print routines
printStrMatrix :: [[String]]->IO()
printStrMatrix [] = printStrList []
printStrMatrix (x:xs) = do
	printStrList x
	printStrMatrix xs

printStrList :: [String] -> IO()
printStrList [] = putStrLn ""
printStrList (x:xs) = do
	putStrLn x
	printStrList xs
-----------------------------------------------
-- utility functions--------------------------------------
-- some utility functions no longer being used but kept here for resouce.
-- states are row major and 0 based indexing
isGoalState :: [String] -> Bool
isGoalState state 
	| null state	= False
	| (state!!2!!(lastcol-1) == 'X')&&(state!!2!!lastcol == 'X') 	
					= True
	| otherwise		= False
--  | otherwise		=  trace ("\nMazeRank :" ++  (show(rateMaze state)) ++" "++ show (state) ++ "\n")  $ False
	where
		lastcol = (length (state!!2)) -1

-- SEARCH POLICY HEURESTIC
-- lower is better,  most important how close is X to goal
-- secondary influence is number of free spaces to right of X
-- which may work against X but helps other get out of the way
-- and is an order of magnitude less important than primary goal
rateMaze:: [String] -> Int
rateMaze state
	| null state 	= 999999
	| otherwise 	= (((length (goalrow)) - xcolumn)*10 - (qtyfreeOnRight))
	where xcolumn = (cpos 'X' (state!!2));
			goalrow = state!!2;
			qtyfreeOnRight = (rfreq '-' (drop xcolumn goalrow))

-- for sort order of search
mazeRank m1 m2
	| (rateMaze m1) > (rateMaze m2) = GT
	| (rateMaze m1) < (rateMaze m2) = LT
	| (rateMaze m1) == (rateMaze m2)= compare m1 m2
	

addUnique :: [String] -> [[String]] -> [[String]]
addUnique [] collect 	= collect
addUnique item collect 	= if (elem item collect) then collect
							else ([item] ++ collect)
							
							
myremoveduplicates list1
	| null list1 = []
	| elem (head list1) (tail list1) = myremoveduplicates (tail list1)
	| otherwise = (head list1):(myremoveduplicates (tail list1))


-- count the number of times a char appears in a string
rfreq :: Char -> String -> Int
rfreq item row 
	|	null row			= 0
	|	item == (head row) 	= 1 + (rfreq item (tail row))
	|	otherwise			= (rfreq item (tail row))
	

isSameLength:: [[a]] -> Bool
isSameLength list
	| null list 		= True
	| (length list) ==1 = True
	| (length (list!!0)) == (length (list!!1))	= True && (isSameLength (drop 1 list))
	| otherwise			= False
	

-- upper left row and col position of car -- important for generating
-- new positions and checking for free space 
-- 0 based row position index, 999 or more means not found = column number
cpos :: Char -> String -> Int
cpos item row
	|	null  row			= 999999 -- return unreasonably large number as flag not found
	|	item==(head row) 	= 0
	|	otherwise = 1 + cpos item (tail row)

-- 0 based row number ,  
rpos:: Char -> [String] -> Int
rpos item maze
	| null maze				= 999999  -- return unreasonably large number as flag for not found
	| elem item(head maze) 	= 0
	| otherwise				= 1 + (rpos item (tail maze))
	
-- note cpos and rpos are zero based.  list indexing in haskell is zero based.  

-- generate a list of cars to base worklist on
getCarList:: [String] -> [Char]
getCarList maze 
	| null maze 	=	[]
	| otherwise		=   myremoveduplicates ((findCar (head maze)) ++ (getCarList (tail maze)))



-- find list of cars in a row, if row car, multiple appearances
findCar:: String -> [Char]
findCar mazerow 
	|	null mazerow 		= []
	| 	(head mazerow) == '-'		= findCar (tail mazerow)
	|	otherwise					= (head mazerow) : (findCar (tail mazerow))

-- given name of car (char) return true if row car
isRowCar:: Char -> String -> Bool
isRowCar car  mazerow
	|	not (elem car mazerow) 	=	error "car not in row"
	|	(rfreq car mazerow) > 1 =	True
	|	otherwise				=	False

-- given name of car and string, return size of car
rowCarSize:: Char -> String -> Int
rowCarSize car mazerow
	|	not (elem car mazerow) 	= 0
	|	otherwise				= (rfreq car mazerow)

	
-- main control path
-- statesearch has been modified to return a set of lists.  The first list is the return
-- path for the current branch being explored.  It may return null if deadend
-- The second list is the visited list which accumulates a list of all visited states
-- the second list is carried over from different branch results into next
-- search paths so that loops are detected across branches.  
----------------------------------------------------------------------------
--statesearch :: [[String]] ->  [[String]] -> [[String]] -> [String] -> ([[String]],[[String]])
statesearch :: [[String]] -> [[String]] -> [[String]] -> [[String] -> [Char] -> [[String]]]  -> ([[String]], [[String]])
statesearch    unexplored     path 			visited       optlist
	| null unexplored	= 
--						trace("end of path search, visited = " ++ (show (length ( visited))) ++ "  path = : " ++ (show (length (path))))  $
						([],(myremoveduplicates (visited ++ path)))
	| isGoalState (head unexplored)	= 
						(([(head unexplored)] ++ path), (myremoveduplicates (visited++path))) -- found it, tack on goal and relay path up chain
	| (elem (head unexplored) (visited ++ path)) = -- move to next unexplored, DO NOT INVOKE generateNewStates, it has been done before - key to loop avoidance : no reference to result allowed - this branch gets hit ALOT....
--						trace( "Duplicate state, visited = " ++ (show (length (visited))) ++ "  path = : " ++ (show (length (path)))   ++ "   unexp : " ++ (show (length unexplored)) ) $ -- ++ (show ( unexplored)))  $
							(statesearch (tail unexplored)
										path   -- we keep same path and just move to next unexplored item
										(myremoveduplicates (visited ++  [(head unexplored)]) )
										optlist) 
	| ((not (null (fst result) )))	= result  -- this invokes generatenewstates 
	| otherwise						=  --this moves on to the next unexplored currently enqueued, rarely gets hit because depth first
--									trace ("otherwise") $ 
										(statesearch (tail unexplored)
												path   -- for backtracking solution
												(visited ++ [(head unexplored)] ++ (snd result))  -- workdown explored list, do not invoke generatenew state
												optlist
										) -- snd result flows visited information back upwards for sharing with other branches
										where result =  
--											trace ("gennewstate") $
												statesearch ((generateNewStates (head unexplored) optlist ))
												([(head unexplored)]++path) -- path expands as we go deeper
												(myremoveduplicates ([(head unexplored)]++visited))
												optlist

-- input is a single unexplored state from which we will generate a list of new states to explore
-- do not block duplicates at this level, statesearch needs to see duplicates so it can control
--generateNewStates :: [String] -> [String] -> [[String]] 
generateNewStates :: [String] -> [[String] -> [Char] -> [[String]]] -> [[String]]
generateNewStates     unexplored optlist
	| unexplored == [] 	= []
	| null optlist 		= []
--	| trace ("carlist = " ++ (show carlist) )  False = undefined
	| otherwise  		= 	 sortBy mazeRank $  -- SEARCH PRIORITY HEURESTIC APPLIED HERE == comment out to run brute force
							((head optlist) unexplored carlist ) ++
							(generateNewStates unexplored (tail optlist))
				where carlist = getCarList unexplored
----------------------------------------------------------------------------------
----------------------------------------------------------------------------- 
-- list operator reference									
-- ghci testing of list of lists 
-- let a = [1,3..21]
-- let b = [11,22..55]
-- let c = [99,98..90]
-- let d = c++b          yields [99,98,97,96,95,94,93,92,91,90,11,22,33,44,55]
-- let d = [c]++[b]      yields [[99,98,97,96,95,94,93,92,91,90],[11,22,33,44,55]]
-- let e = d++[a]        to yield a list of 3 lists
-- must manually ensure we explictly deal with list, or list of list appropriately
-- elem a e              True
-- elem b e              True
-- elem c e              True
-- let g = a++[2]; elem g e    False          will be really useful
-- let f=e
-- f==e                  True      works on comparing list of list as you would expect.
-- e==d                  False
-----------------------------------------------------------------------------
-- The left move methods

-- move all cars left 
moveAllCarsLeft :: [String]  -> String -> [[String]]
moveAllCarsLeft	unexplored carlist 
	| null carlist 	= []
	| otherwise 	= if (null newstate)
						then filter (not.null) (moveAllCarsLeft unexplored  (tail carlist)) 
						else filter (not.null)([newstate] ++ (moveAllCarsLeft unexplored (tail carlist))) 
	where newstate = (moveCarLeft (head carlist) unexplored  )

-- move one car left and generate a new maze state
-- this was the place that we use to check for duplicates in visited but no longer required
-- so its sole purpose is to kick of row by row matrix build for new state
moveCarLeft :: Char -> [String] ->  [String]
moveCarLeft    car     unexpl  
	| otherwise					= newstate
	where
		newstate =   moveCarLeftInMaze car unexpl []

-- move one car left in a maze by processing each row of the maze seperately
moveCarLeftInMaze:: Char -> [String] ->  [String] -> [String]
moveCarLeftInMaze car unexpl  result
	| null unexpl 	= result
	| (elem car (head unexpl)) = if (null newrow) 
									then []
									else (moveCarLeftInMaze car (tail unexpl) (result ++[newrow]) )
	| otherwise		= (moveCarLeftInMaze car (tail unexpl) (result++[(head unexpl)]))
	where 
		newrow = (moveLCar car (head unexpl) [] [] ) 

		
-- given a row, move the car in the row and return the new row
moveLCar:: Char -> String -> String-> String -> String
moveLCar   c       row       preCar   car
    | ((null row)&&(length car)>1) 	= (preCar ++ car ++ "-") --reached end of row while building car, min size reached
	| ((null row)&&(length car)<2)	= []  -- reached end of row before complete car found
	| ((length(car)<1)&&((head row) == '-')&&((head(tail row))==c)) -- found potential moveable car
											= moveLCar c (tail (tail row)) preCar (car++[c]) 
	| ((length(car)<1)&&((head row) == c))  = [] -- car at front, no room to move
	| (((length car)==1) && ((head row)/=c))= [] -- not a row car, only one element
	| (((length car)>1) && ((head row)/=c))	= (preCar ++ car ++ "-" ++ row) -- reached end of car- complete
	| (null (tail row))						= (preCar ++ car ++ [c] ++ "-")  -- last char in row is a car while building a car
	| ((head row) == c)						= moveLCar c (tail row) preCar (car++[c]) -- already have a car piece, heres another
	| otherwise 							= moveLCar c (tail row) (preCar++[(head row)])  car --no car yet, keep moving row to preCar
	

-- right move methods
-----------------------------------------------------------------------------
-- generates all right moves for all cars
moveAllCarsRight :: [String]  -> String -> [[String]]
moveAllCarsRight	unexplored carlist 
	| null carlist 	= []
	| otherwise 	= if (null newstate) then filter (not.null) (moveAllCarsRight unexplored  (tail carlist)) 
						else filter (not.null) (newstate	++  (moveAllCarsRight unexplored  (tail carlist))) 
	where 
		newstate = [(moveCarRight (head carlist) unexplored )]

-- generates one new maze state for one car move to the right 
moveCarRight :: Char -> [String] ->  [String]
moveCarRight   car     unexpl             
	| otherwise					= newstate
	where
		newstate = moveCarRightInMaze car unexpl []

-- move one car right in a maze by looping through each row of a maze
moveCarRightInMaze:: Char -> [String] ->  [String] -> [String]
moveCarRightInMaze car unexpl  result
	| null unexpl 	= result
	| (elem car (head unexpl)) = if (null newrow) 
									then []
									else (moveCarRightInMaze car (tail unexpl) (result++[newrow]) )
	| otherwise		=  (moveCarRightInMaze car (tail unexpl) (result++[(head unexpl)]))
	where 
		newrow = (moveRCar car (head unexpl) [] [] ) 
		
-- move car to right within one row of a maze and return that row
moveRCar:: Char -> String -> String-> String -> String
moveRCar   c       row       preCar   car
	| null row			= []
	| ((length  car < 1)&&((head row) /= c))					= (moveRCar c (tail row) (preCar++[(head row)]) car) --store stuff before car
	| (((head row) /= c ) && (length (car)==1)) 				= []  -- not a row car
	| (((head row) /= c ) && (head(row)/='-')&&((length car)>1))= []  -- found it but blocked
	| (((head row) == '-' ) && ((length car)>1))  				= (preCar++['-']++car++(tail row))  -- found it and good to move
	| ((head row) == c)							 				= (moveRCar c (tail row) preCar (car++[c])) -- found car keep building car
	| otherwise 	= (moveRCar c (tail row) (preCar ++ [(head row)]) car)  -- carlength>1,rowhead not c or -. blocked
	

-- move down methods
--------------------------------------------------------------
moveAllCarsDown :: [String] -> String -> [[String]]
moveAllCarsDown	unexplored carlist 
	| null carlist 	= []
	| otherwise 	= if (null newstate) then filter (not.null) (moveAllCarsDown unexplored (tail carlist)) 
						else filter (not.null) (newstate	++  (moveAllCarsDown unexplored (tail carlist))) 
	where 
		newstate = [(moveCarDown (head carlist) unexplored )]
-- note command line behavoir for appending null list to list of lists does not seem
-- consistent with what happens programmatically [[a]] ++ [b] -> [[a],[b]], [[a]] ++ [] -> [[a]] on ghci but here it is [[a],[]]
		
		
-- generates one new maze state for one car move to the Down 
-- moving down is the same is moving right on the transpose of matrix
-- reuse moveCarRightInMaze as entrypoint

moveCarDown :: Char -> [String]  -> [String]
moveCarDown   car     unexpl      
	| otherwise					= newstate
	where
		newstate = (transpose (moveCarRightInMaze car (transpose unexpl) [] ))

-- move up methods
--------------------------------------------------------------	

moveAllCarsUp :: [String] -> String -> [[String]]
moveAllCarsUp	unexplored carlist 
	| null carlist 	= []
	| otherwise 	= if (null newstate) then filter (not.null) (moveAllCarsUp unexplored  (tail carlist)) 
--	| otherwise 	= trace ("moveallcarup: " ++ (show newstate) ++ "," ++(show carlist)  ++ "\n") $ if (null newstate) then filter (not.null) (moveAllCarsUp unexplored  (tail carlist)) 
						else filter (not.null) (newstate	++  (moveAllCarsUp unexplored  (tail carlist))) 
	where 
		newstate = [(moveCarUp (head carlist) unexplored )]

		
		
-- generates one new maze state for one car move to the Up 
-- moving Up is the same is moving right on the transpose of matrix
-- reuse moveCarLeftInMaze as entrypoint

moveCarUp :: Char -> [String] -> [String]
moveCarUp   car     unexpl     
	| otherwise					= newstate
	where
		newstate = (transpose (moveCarLeftInMaze car (transpose unexpl) []))
