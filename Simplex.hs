module Simplex
(
	getInclusiveVariable
)
where
 
import Numeric.LinearAlgebra
import Foreign.Storable
import HMatrixUtil
import MyTrace

data SimplexTask = SimplexTask (Matrix Double)

defaultDecimalPlaces :: Int
defaultDecimalPlaces = 2
defaultVecdisp = vecdisp (dispf defaultDecimalPlaces)

class HasZString t where
	getZString:: t -> Vector Double
	
instance HasZString SimplexTask where
	getZString (SimplexTask theMatrix) = getRow (rows theMatrix - 1) theMatrix

instance Show SimplexTask where
	show (SimplexTask theMatrix) = 
		dispf defaultDecimalPlaces theMatrix
		
class HasConstraints t where
	getConstraints :: t -> Matrix Double
	
instance HasConstraints SimplexTask where
	getConstraints (SimplexTask theMatrix) = takeRows (rows theMatrix - 2) theMatrix
		
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
					 -5, -4, 0, 0, 0, 0, 0::Double
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

simplex :: SimplexTask -> Vector Double
simplex = simplexIterate simplexIterationLimit

simplexIterate :: Int -> SimplexTask -> Vector Double
simplexIterate iterationsLeft task@(SimplexTask theMatrix)
	| iterationsLeft <= 0 = getZString task
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
					getZString task
				else
					simplexIterate (iterationsLeft - 1) nextTask
			)
	where
		inclusiveVariableIndex = getInclusiveVariable task -- column
		exclusiveVariableIndex = getExclusiveVariable task inclusiveVariableIndex -- row
		inclusiveValue = theMatrix @@> (exclusiveVariableIndex, inclusiveVariableIndex)
		nextTask = (SimplexTask newMatrix)
		newMatrix = buildMatrix (rows theMatrix) (cols theMatrix) createNewConstraints
		createNewConstraints (rowIndex, columnIndex) = 
			if 
				rowIndex == exclusiveVariableIndex
			then
				currentValue / inclusiveValue
			else
				currentValue - currentValueAtExclusiveString * currentValueAtIndlusiveColumn
			where
				currentValue = theMatrix @@> (rowIndex, columnIndex)
				currentValueAtExclusiveString = theMatrix @@> (exclusiveVariableIndex, columnIndex)
				currentValueAtIndlusiveColumn = theMatrix @@> (rowIndex, inclusiveVariableIndex)
	
evaluate :: Vector Double -> Vector Double -> Double
evaluate function arguments = function <.> arguments

