module Simplex2ndEdition
(
)
where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import MyTrace

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
data MElement = MElement Element Element deriving (Show, Eq)

instance Ord MElement where
	compare (MElement a aM) (MElement b bM) = 
		if 
			aM /= bM
		then
			compare aM bM
		else
			compare a b

data Direction = Maximize | Minimize

data Task = Task (Matrix MElement) Direction

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
		(Task
			(getTaskMatrixFromElementLists
				[
					[3, 2, -1, 0],
					[2, 3, 0, -1],
					[-5, -4, 0, 0]
				]
			)
			Minimize
		)
	]

-- each value in the result vector corresponds to a row. False means that this row needs a fictional variable
evaluateSimplexInitialSolution :: Task -> Vector Bool
evaluateSimplexInitialSolution task =
	trace (show (nrows constraints)) (V.generate (nrows constraints) (\ index -> isRowEquable (index + 1)))
	where
		isRowEquable rowIndex = 
			hasPositiveNonTargetVariable (getRow rowIndex constraints)
		hasPositiveNonTargetVariable row = 
			go 0
			where
				go i =
					if 
						i < V.length row
					then
						current || (go (i + 1))
					else
						False
					where
						current = ((targetVector V.! i) == (MElement 0 0)) && ((row V.! i) > 0)
		constraints = getConstraints task
		targetVector = getTargetVector task
