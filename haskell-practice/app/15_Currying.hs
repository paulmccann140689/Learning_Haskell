-- Currying refers to the transformation of a function that takes multiple arguments into a series of functions 
-- that each take a single argument. This makes it possible to apply arguments to a function one at a time, 
-- a concept known as partial application.

-- In Haskell, all functions are curried by default, even if they appear to take multiple arguments. 
-- For instance, a function like add x y = x + y is actually a function that takes one argument (x), 
-- and returns another function that takes the second argument (y).

-- A function like addCurried :: Int -> Int -> Int can be thought of as:
-- addCurried :: Int -> (Int -> Int)

-- The function takes an Int as input.
-- It returns a new function of type Int -> Int, which takes another Int and returns the result.
-- When you partially apply a function, you are "fixing" some of its arguments and returning a new function 
-- that still requires the remaining arguments. This is what makes currying and partial application so powerful in Haskell

-- In Haskell, all functions are curried by default.
-- This means that every function actually takes one argument at a time and returns a function that takes the next argument.
-- This is what "currying" means in Haskell.


-- For example, consider the same add function in Haskell:

-- add :: Int -> Int -> Int
-- add x y = x + y

-- You might think `add` takes two arguments (x and y), but due to currying, Haskell interprets it as two single-argument functions like this:

-- add :: Int -> (Int -> Int)

-- This means:
-- 1. `add` is a function that takes one integer (x) and returns a new function.
-- 2. The returned function takes one more integer (y) and then returns the result (x + y).

-- Detailed Example: Currying in Action with add

-- Calling the function: add 3 5

-- First Step (add 3):
-- When you call `add 3`, Haskell doesn’t yet compute the sum. 
-- Instead, it returns a new function that takes the second argument. This new function is like this:

-- add 3 = (\y -> 3 + y)

-- This means `add 3` returns a function that takes one argument `y` and adds 3 to it.

-- Second Step (add 3 5):
-- Now, we apply this new function `\y -> 3 + y` to the second argument `5`. This gives:

-- add 5 = (\y -> 3 + y) 5

-- When this function is applied to `5`, the result is:

-- 3 + 5 = 8

-- So `add 3 5` evaluates to `8`.

-- Visualizing the Steps
-- Think of it as a two-step process:
-- 1. `add 3` returns a new function that looks like this: `\y -> 3 + y`.
-- 2. When you give this function the second argument (`5`), it returns `3 + 5 = 8`.

-- Partial Application: Applying Part of a Function
-- One of the biggest advantages of currying is partial application. 
-- This means you can apply some of the arguments of a function and get back a new function that still expects the rest of the arguments.

-- For example:
-- add3 :: Int -> Int
-- add3 = add 3

-- Now, `add3` is a partially-applied function that waits for the second argument.
-- Example: add3 5 is equivalent to add 3 5, and will return 8.
