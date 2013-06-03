module Simplex
(
	getInclusiveVariable
)
where
 
import Numeric.LinearAlgebra
import Foreign.Storable
import HMatrixUtil
import MyTrace

data SimplexTask = SimplexTask (Matrix Double) (Vector Double)

defaultDecimalPlaces :: Int
defaultDecimalPlaces = 2
defaultVecdisp = vecdisp (dispf defaultDecimalPlaces)

instance Show SimplexTask where
	show (SimplexTask constraints zString) = 
		"Constraints: "
		++ dispf defaultDecimalPlaces constraints
		++ "ZString: \n"
		++ defaultVecdisp zString

sampleTasks = 
	[
		(SimplexTask
			(
				(4 >< 7)
				[
					 6, 4, 1, 0, 0, 0,  24,
					 1, 2, 0, 1, 0, 0,  6,
					-1, 1, 0, 0, 1, 0,  1,
					 0, 1, 0, 0, 0, 1,  2
				]
			)
			(
				7 |> [-5, -4, 0, 0, 0, 0, 0]
			)
		)
	]

--this function returns column index
getInclusiveVariable :: SimplexTask -> Int
getInclusiveVariable (SimplexTask constraints zString) =
	resultIndex
	where
		index = searchVector zString vectorChooseMin
		value = zString @> index
		resultIndex = 
			if
				value < 0
			then
				index
			else
				-1

--this function returns row index
getExclusiveVariable :: SimplexTask -> Int -> Int
getExclusiveVariable (SimplexTask constraints zString) inclusiveVariableIndex =
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

simplex :: SimplexTask -> Vector Double
simplex task@(SimplexTask constraints zString) =
	zString
	
evaluate :: Vector Double -> Vector Double -> Double
evaluate function arguments = function <.> arguments
	
findInclusiveVariable :: Vector t -> Int
findInclusiveVariable target =
	0
