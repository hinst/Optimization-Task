module Simplex2ndEdition
(
)
where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.Ratio
import Data.Maybe
import MyTrace
import VectorUtil

type Matrix elementType = M.Matrix elementType
submatrix = M.submatrix
nrows = M.nrows
ncols = M.ncols
getRow = M.getRow
getCol = M.getCol
fromLists = M.fromLists
type Vector elementType = V.Vector elementType

type Element = Rational

-- first is plain number, second is M*number
data MElement = MElement Element Element deriving (Eq)

class HasSign t where
	getSign :: t -> Int
	
	-- additional functions below
	isPositive :: t -> Bool
	isPositive x = getSign x == 1
	
	isZero :: t -> Bool
	isZero x = getSign x == 0
	
	isNegative :: t -> Bool
	isNegative x = getSign x == -1
	
instance HasSign MElement where
	getSign (MElement value mValue)
		| (mValue > 0) = 1
		| (mValue < 0) = -1
		| (value > 0) && (mValue == 0) = 1
		| (value < 0) && (mValue == 0) = -1
		| (value == 0) && (mValue == 0) = 0

defaultShowRational :: Rational -> String
defaultShowRational x
	| multiplier == 0 = "0"
	| divisor == 1 = show multiplier
	| otherwise = show multiplier ++ "/" ++ show divisor
	where
		multiplier = numerator x
		divisor = denominator x
	
instance Show MElement where
	show (MElement value mValue) = 
		defaultShowRational value 
		++ 
		if
			mValue /= 0
		then
			(
				if
					mValue > 0
				then
					"+"
				else
					"-"
			)
			++
			defaultShowRational (abs mValue)
			++ 
			"*M"
		else
			""

instance Ord MElement where
	compare (MElement a aM) (MElement b bM) =
		if 
			aM /= bM
		then
			compare aM bM
		else
			compare a b

data Direction = Maximize | Minimize deriving (Eq, Show)

data Task = Task (Matrix MElement) Direction

instance Show Task where
	show (Task matrix direction) = show matrix ++ show direction

getMatrix :: Task -> Matrix MElement
getMatrix (Task matrix _) = matrix

getDirection :: Task -> Direction
getDirection (Task _ direction) = direction

getConstraints :: Task -> Matrix Element
getConstraints (Task m _) = 
	result
	where
		matrix = submatrix 1 (rowCount - 1) 1 columnCount m
		result = M.matrix (nrows matrix) (ncols matrix) transform
		transform (rowIndex, columnIndex) = 
			plainElement
			where
				(MElement plainElement _) = matrix M.! (rowIndex, columnIndex)
		rowCount = nrows m
		columnCount = ncols m
		
getTargetVector (Task m _) =
	getRow (rowCount) m
	where
		rowCount = nrows m
		
getLeftMatrix :: Task -> Matrix MElement
getLeftMatrix task@(Task matrix _) =
	submatrix 1 (nrows matrix) 1 (ncols matrix - 1) matrix
	
getRightMatrix :: Task -> Matrix MElement
getRightMatrix task@(Task matrix _) =
	submatrix 1 (nrows matrix) (ncols matrix) (ncols matrix) matrix

getTaskMatrixFromElementLists :: [[Element]] -> Matrix MElement
getTaskMatrixFromElementLists lists = 
	M.matrix rowCount columnCount transform
	where
		initialMatrix = fromLists lists
		rowCount = nrows initialMatrix
		columnCount = ncols initialMatrix
		transform (rowIndex, columnIndex) = 
			(MElement (initialMatrix M.! (rowIndex, columnIndex)) 0)

sampleTasks :: [Task]
sampleTasks = 
	[
		(Task -- Homework task
			(getTaskMatrixFromElementLists
				[
					[3, 2, -1, 0, 5],
					[2, 3, 0, -1, 7],
					[-5, -4, 0, 0, 0]
				]
			)
			Minimize
		),
		(Task -- Reddy Mikks task from book @ page 107, the answer is @ page 111
			(getTaskMatrixFromElementLists
				[
					[6, 4, 1, 0, 0, 0, 24],
					[1, 2, 0, 1, 0, 0, 6],
					[-1, 1, 0, 0, 1, 0, 1],
					[0, 1, 0, 0, 0, 1, 2],
					[-5, -4, 0, 0, 0, 0, 0]
				]
			)
			Maximize
		)

	]

-- each value in the result vector corresponds to a row. False means that this row needs a fictional variable
evaluateSimplexInitialSolution :: Task -> Vector Bool
evaluateSimplexInitialSolution task =
	V.generate (nrows constraints) (\ index -> isRowEquable (index + 1))
	where
		isRowEquable rowIndex = 
			( (row V.! (rowLength - 1)) == 0 ) -- does not needs
			||
			hasPositiveNonTargetVariable
			where
				row = getRow rowIndex constraints
				rowLength = V.length row
				hasPositiveNonTargetVariable = 
					go 0
					where
						go i =
							if 
								i < rowLength
							then
								currentResult || (go (i + 1))
							else
								False
							where
								currentResult = 
									currentIsNonTarget 
									&& 
									currentIsPositive 
									&& 
									currentIsNotAConstraint
								currentIsNonTarget = isZero (targetVector V.! i)
								currentIsPositive = ((row V.! i) > 0)
								currentIsNotAConstraint = i < (rowLength - 1)
						
						
		constraints = getConstraints task
		targetVector = getTargetVector task
		
-- Эта функция получает вектор, в котором на месте #i стоит False если в строке #i требуется искусственная переменная
-- Эта функция возвращает список колонок, которые следует приписать справа к Симплекс-матрице, чтобы ввести в неё искусственные переменные M...
buildFictionalVariableColumns :: Vector Bool -> [Vector MElement]
buildFictionalVariableColumns satisfactory =
	go 0
	where
		n = V.length satisfactory
		go i =
			if
				i < n
			then
				current ++ go (succ i)
			else
				[]
			where
				current	=
					if
						not (satisfactory V.! i)
					then
						[vector]
					else
						[]
					where
						vector = V.generate n getValue
						getValue i_ = 
							if
								i_ == i
							then
								(MElement 1 0)
							else
								(MElement 0 0)

buildFictionalVariableBlock :: Task -> Maybe (Matrix MElement)
buildFictionalVariableBlock task@(Task matrix direction) =
	if 
		fictionalVariablesRequired
	then
		Just (M.matrix rowCount columnCount build)
	else
		Nothing
	where
		rowCount = nrows matrix
		columnCount = length list
		list = buildFictionalVariableColumns (evaluateSimplexInitialSolution task)
		fictionalVariablesRequired = list /= []
		build (rowIndex, columnIndex)
			| rowIndex < rowCount = (list !! (columnIndex - 1)) V.! (rowIndex - 1)
			| otherwise = 
				if 
					direction == Minimize
				then
					(MElement 0 (-1))
				else
					(MElement 0 1)
					
ensureFictionalVariables :: Task -> Task
ensureFictionalVariables task@(Task matrix direction) = 
	result
	where
		block = buildFictionalVariableBlock task
		result =
			if
				isJust block
			then
				(Task (performFictionalCorrection newMatrix) direction)
			else
				task
			where
				newMatrix =
					trace
						(
							"Simplex matrix with fictional variables before correction:\n" 
							++ 
							(show result)
						)
						result
					where
						result =
							left M.<|> fromJust block M.<|> right -- insert fictional block
							where
								left = getLeftMatrix task
								right = getRightMatrix task
		performFictionalCorrection matrix =
			M.matrix (nrows matrix) (ncols matrix) build
			where
				build index@(rowIndex, columnIndex)
					| rowIndex < (nrows matrix) = current
					| otherwise = MElement value correctedMValue
					where
						correctedMValue = 
							mValue + upperResult
							where
								upperResult = (upperVSum (rowIndex - 1))
						current@(MElement value mValue) = matrix M.! index
						upperVSum i =
							if 
								i > 0
							then
								value + upperVSum (i - 1)
							else
								0
							where
								(MElement value _) = matrix M.! (i, columnIndex)

simplexChooseColumn :: Task -> Int
simplexChooseColumn task@(Task matrix direction) = 
	result
	where
		(getIndex, checkValue) = 
			case direction of
				Minimize -> (V.maxIndex, isPositive) -- && > 0
				Maximize -> (V.minIndex, isNegative) -- && < 0
		targetVector = getTargetVector task
		index = getIndex (V.take (V.length targetVector - 1) targetVector)
		 -- one should not take the last element because it does not correspond to a variable
		value = targetVector V.! index
		result = 
			if 
				checkValue value
			then
				index
			else
				-1

-- requires column index
simplexChooseRow :: Task -> Int -> Int
simplexChooseRow task chosenColumnIndex =
	0
	where
		matrix = getConstraints task
		ratios = V.generate (nrows matrix) generateRatio
		generateRatio index =
			if 
				0 == cccv
			then
				-1
			else
				crcv / cccv
			where
				-- cccv means current chosen column value
				cccv = matrix M.! (index, chosenColumnIndex)
				-- crcv means current right column value
				crcv = matrix M.! (index, ncols matrix)

simplexPhaseForward :: Task -> Task
simplexPhaseForward task = 
	task
