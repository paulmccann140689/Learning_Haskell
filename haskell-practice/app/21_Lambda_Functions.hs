-- Lambda Functions
-- In Haskell, a lambda function (or anonymous function) is a function defined without a name. 
-- Lambda functions are often used when you need a simple, short function and don't want to define a full named function. 
-- They can be especially useful in cases where you need to pass a small function as an argument 
-- to another function or for quick one-time use.

-- Syntax for Lambda Functions
-- \parameters -> expression

-- \ introduces the lambda function.
-- parameters are the inputs or arguments to the function.
-- -> separates the parameters from the body (the expression to be evaluated).

-- \x -> expression ,means "a function that takes one argument x and returns expression."
-- Similarly, \y -> expression  ,means "a function that takes one argument y and returns expression."


--Example:
add :: Int -> Int -> Int
add x y = x + y
-- When we apply the first argument `3` to `add`, Haskell returns this lambda function:
-- \y -> 3 + y
add3 = add 3 -- equivalent to \y -> 3 + y
-- Now `add3` is a function waiting for one more argument.
-- Calling `add3 5` would result in: (\y -> 3 + y) 5 = 3 + 5 = 8
result = add3 5 -- Result is 8


-- Add 5 to a number
lambdaExample1 :: Int
lambdaExample1 = (\n -> n + 5) 10
-- Usage: lambdaExample1  -- Result: 15


-- Multiply by 3
lambdaExample2 :: Int
lambdaExample2 = (\x -> x * 3) 7
-- Usage: lambdaExample2  -- Result: 21


-- Check if a number is even
lambdaExample3 :: Bool
lambdaExample3 = (\n -> n `mod` 2 == 0) 4
-- Usage: lambdaExample3  -- Result: True
-- checking a modular value must always yield a boolean result (==0 for example)


-- Double a list of numbers
lambdaExample4 :: [Int]
lambdaExample4 = map (\x -> x * 2) [1, 2, 3, 4, 5]
-- Usage: lambdaExample4  -- Result: [2, 4, 6, 8, 10]


-- Filter odd numbers
lambdaExample5 :: [Int]
lambdaExample5 = filter (\x -> x `mod` 2 /= 0) [1, 2, 3, 4, 5, 6]
-- Usage: lambdaExample5  -- Result: [1, 3, 5]


-- Sum of a list
lambdaExample6 :: Int
lambdaExample6 = foldl (\acc x -> acc + x) 0 [1, 2, 3, 4]
-- Usage: lambdaExample6  -- Result: 10


-- Fibonacci Sequence using a lambda inside a 'let' binding
lambdaExample7 :: Int
lambdaExample7 = let fib = \n -> if n == 0 
                                 then 0 
                                 else if n == 1 
                                 then 1 
                                 else fib (n - 1) + fib (n - 2)
                 in fib 6
-- Usage: lambdaExample7  -- Result: 8


-- Find maximum in a list
lambdaExample8 :: Int
lambdaExample8 = foldl1 (\acc x -> if x > acc then x else acc) [3, 9, 2, 5, 6]
-- Usage: lambdaExample8  -- Result: 9


-- Create a list of pairs
lambdaExample9 :: [(Int, Int)]
lambdaExample9 = zipWith (\x y -> (x, y)) [1, 2, 3] [4, 5, 6]
-- Usage: lambdaExample9  -- Result: [(1,4), (2,5), (3,6)]