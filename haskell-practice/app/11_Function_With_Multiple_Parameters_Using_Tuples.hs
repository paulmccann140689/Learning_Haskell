-- In Haskell, functions can accept multiple parameters in various ways, including tuples. 
-- A tuple is a fixed-size collection of values of potentially different types. 
-- When a function takes multiple parameters packed into a tuple, all the arguments are passed as a single tuple, 
-- and the function extracts the values from the tuple to operate on them.

-- How Tuple Functions Work
-- When defining a function with a tuple as a parameter, we typically:

-- Specify the type of the tuple in the function signature.
-- Pattern match the tuple inside the function definition to extract its values.
-- Operate on the values and return the result.

-- Basic Syntax

-- Function with a tuple (param1, param2) as input
-- functionName :: (Type1, Type2) -> ReturnType
-- functionName (param1, param2) =  (do something with param1 and param2)

-- Sum of two integers using a tuple:
sumTuple :: (Int, Int) -> Int
sumTuple (a, b) = a + b
-- sumTuple (3, 5)   -- Result: 8


-- Concatenate two strings:
concatTuple :: (String, String) -> String
concatTuple (s1, s2) = s1 ++ s2
-- concatTuple ("Hello, ", "World!")  -- Result: "Hello, World!"


-- Boolean AND operation using a tuple:
andTuple :: (Bool, Bool) -> Bool
andTuple (b1, b2) = b1 && b2
-- andTuple (True, False)  -- Result: False


-- Distance between two points (2D):
distance2D :: ((Float, Float), (Float, Float)) -> Float
distance2D ((x1, y1), (x2, y2)) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
-- distance2D ((1.0, 2.0), (4.0, 6.0))  -- Result: 5.0


-- Swap elements in a tuple:
swapTuple :: (a, b) -> (b, a)
swapTuple (x, y) = (y, x)
-- swapTuple (5, "Haskell")  -- Result: ("Haskell", 5)


-- Apply two functions to a tuple of two elements:
applyFunctions :: (a -> b, c -> d) -> (a, c) -> (b, d)
applyFunctions (f, g) (x, y) = (f x, g y)
-- applyFunctions ((+1), (*2)) (3, 4)  -- Result: (4, 8)


-- Dot product of two 3D vectors:
dotProduct3D :: ((Float, Float, Float), (Float, Float, Float)) -> Float
dotProduct3D ((x1, y1, z1), (x2, y2, z2)) = x1 * x2 + y1 * y2 + z1 * z2
-- dotProduct3D ((1.0, 2.0, 3.0), (4.0, 5.0, 6.0))  -- Result: 32.0


-- Finding the minimum and maximum of a list using a tuple:
minMax :: Ord a => [a] -> (a, a)
minMax [] = error "Empty list"
minMax xs = (minimum xs, maximum xs)
-- minMax [3, 1, 4, 1, 5, 9]  -- Result: (1, 9)


-- Solving a system of linear equations using a tuple:
solveLinearSystem :: ((Float, Float, Float), (Float, Float, Float)) -> (Float, Float)
solveLinearSystem ((a1, b1, c1), (a2, b2, c2)) = 
  let det = a1 * b2 - a2 * b1
  in if det == 0
     then error "No unique solution"
     else ((c1 * b2 - c2 * b1) / det, (a1 * c2 - a2 * c1) / det)
-- a1 * x + b1 * y = c1
-- a2 * x + b2 * y = c2
-- solveLinearSystem ((1, 2, 9), (3, 4, 24))  -- Result: (6.0, 1.5)