module Simplex
(
)
where
 
import Numeric.LinearAlgebra
import Foreign.Storable
import MyTrace

data SimplexTask t = SimplexTask (Matrix t) (Vector t)

sampleTasks = 
	[
		(SimplexTask
			(
				(5 >< 7)
				[
					 6, 4, 1, 0, 0, 0,  24,
					 1, 2, 0, 1, 0, 0,  6,
					-1, 1, 0, 0, 1, 0,  1,
					 0, 1, 0, 0, 0, 1,  2
				]
			)
			(-5, -4, 0, 0, 0, 0)
		)
	]

simplex :: (Product t) => SimplexTask t -> Vector t
simplex task =
	0 |> ([]::[t])
	
evaluate :: (Product t) => Vector t -> Vector t -> t
evaluate function arguments = function <.> arguments
	
findInclusiveVariable :: Vector t -> Int
findInclusiveVariable target =
	0
	

	
					
			
