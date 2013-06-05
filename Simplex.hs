module Simplex
(
	getInclusiveVariable
)
where
 
import Numeric.LinearAlgebra
import Foreign.Storable
import HMatrixUtil
import MyTrace

type SimplexTaskElement = Double

data SimplexTask = SimplexTask (Matrix SimplexTaskElement)

defaultDecimalPlaces :: Int
defaultDecimalPlaces = 3
defaultVecdisp = vecdisp (dispf defaultDecimalPlaces)

getZString (SimplexTask theMatrix) = getRow (rows theMatrix - 1) theMatrix

instance Show SimplexTask where
	show (SimplexTask m) = 
		dispf defaultDecimalPlaces m
		
getConstraints (SimplexTask theMatrix) = takeRows (rows theMatrix - 2) theMatrix
	
showSimplexTaskFractional :: SimplexTask -> String
showSimplexTaskFractional (SimplexTask theMatrix) = 
	go 0 0 ++ "\n"
	where
		go rowIndex columnIndex =
			if 
				(columnIndex < columnCount)
			then
				current ++ " " ++ go rowIndex (columnIndex + 1)
			else
				"\n"
				++
				if 
					(rowIndex < rowCount - 1)
				then
					go (rowIndex + 1) 0
				else
					""
			where
				rowCount = rows theMatrix
				columnCount = cols theMatrix
				current = show (currentValue)
				currentValue = theMatrix @@> (rowIndex, columnIndex)
		
sampleTasks = 
	[
		(SimplexTask
			(
				(5 >< 7)
				[
					 6, 4, 1, 0, 0, 0,  24,
					 1, 2, 0, 1, 0, 0,  6,
					-1, 1, 0, 0, 1, 0,  1,
					 0, 1, 0, 0, 0, 1,  2,
					 -5, -4, 0, 0, 0, 0, 0::SimplexTaskElement
				]
			)
		)
	]

--this function returns column index
getInclusiveVariable :: SimplexTask -> Int
getInclusiveVariable task =
	resultIndex
	where
		index = searchVectorS zString vectorChooseMin
		value = zString @> index
		zString = getZString task
		resultIndex = 
			if 
				value < 0
			then
				index
			else
				-1
				

--this function returns row index
getExclusiveVariable :: SimplexTask -> Int -> Int
getExclusiveVariable task inclusiveVariableIndex =
	resultIndex
	where
		traceEnabled = False
		trace logMessage expression = 
			if 
				traceEnabled
			then
				MyTrace.trace logMessage expression
			else
				expression
		resultIndex = 
			if 
				inclusiveVariableIndex /= -1
			then
				bestRatio			
			else
				-1
		constraints = getConstraints task
		inclusiveVariableColumn = getColumn inclusiveVariableIndex constraints
		rightColumn = getColumn (cols constraints - 1) constraints
		ratioColumn = 
			trace
				(
					defaultVecdisp rightColumn 
					++ defaultVecdisp inclusiveVariableColumn 
					++ defaultVecdisp result
				)
				result
			where
				result = mapVectorWithIndex evaluateRatio inclusiveVariableColumn
				evaluateRatio index inclusiveValue
					| inclusiveValue == 0 = -1
					| otherwise = rightValue / inclusiveValue
					where
						rightValue = rightColumn @> index
		bestRatio = 
			searchVectorS ratioColumn chooseRatio
		chooseRatio a b
			| (a > 0) && (b > 0) = if a <= b then 0 else 1
			| (a > 0) && (b <= 0) = 0
			| (a <= 0) && (b > 0) = 1
			| (a < 0) && (b < 0) = -1

simplexIterationLimit = 3

simplex :: SimplexTask -> Vector SimplexTaskElement
simplex = simplexIterate simplexIterationLimit

simplexIterate :: Int -> SimplexTask -> Vector SimplexTaskElement
simplexIterate iterationsLeft task@(SimplexTask theMatrix)
	| iterationsLeft <= 0 = 
		trace
			"Iteration limit reached. Exiting."
			(getZString task)
	| otherwise = 
		trace
			(
				"Next iteration of the simplex method: task:\n"
				++
				show task
			)
			(
				if 
					inclusiveVariableIndex == -1 --last iteraion
				then
					trace
						"Solution found. Exiting."
						(getZString task)
				else
					simplexIterate (iterationsLeft - 1) nextTask
			)
	where
		inclusiveVariableIndex = getInclusiveVariable task -- column
		exclusiveVariableIndex = getExclusiveVariable task inclusiveVariableIndex -- row
		inclusiveValue = theMatrix @@> (exclusiveVariableIndex, inclusiveVariableIndex)
		nextTask = (SimplexTask newMatrix)
		newExclusiveString = buildVector (cols theMatrix) getNewExclusiveStringValue
		getNewExclusiveStringValue columnIndex = 
			theMatrix @@> (exclusiveVariableIndex, columnIndex) / inclusiveValue
		newMatrix = buildMatrix (rows theMatrix) (cols theMatrix) getNewTaskMatrixValue
		getNewTaskMatrixValue (rowIndex, columnIndex) = 
			if 
				rowIndex == exclusiveVariableIndex
			then
				newExclusiveString @> columnIndex
			else
				currentValue - currentValueAtNewExclusiveString * currentValueAtInclusiveColumn
			where
				currentValue = theMatrix @@> (rowIndex, columnIndex)
				currentValueAtNewExclusiveString = newExclusiveString @> columnIndex
				currentValueAtInclusiveColumn = theMatrix @@> (rowIndex, inclusiveVariableIndex)
	
evaluate :: Vector SimplexTaskElement -> Vector SimplexTaskElement -> SimplexTaskElement
evaluate function arguments = function <.> arguments

