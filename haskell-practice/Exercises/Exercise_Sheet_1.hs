-- 1 Write a function max2 :: Int -> Int -> Int that takes two integers as its arguments and returns the maximum value.
max2 :: Int -> Int -> Int
max2 x y = if x > y then x else y


-- 2. Using your max2 function, write a function max3 :: Int -> Int -> Int -> Int that returns the
-- maximum of three numbers.
max3 :: Int -> Int -> Int -> Int
max3 x y z = max x $ max y z        -- Can also be written as max x (max y z).  

-- The $ avoids the use of parenthesis due to it's low precedence

-- Type Signature:
-- max3 :: Int -> Int -> Int -> Int: This means that max3 is a function that takes three Int arguments and returns an Int.

-- Function Body:
-- max3 x y z = max x $ max y z

-- The inner part max y z computes the maximum of y and z.
-- The outer part max x $ ... computes the maximum of x and the result of max y z.

-- When you call max3 x y z, the function evaluates max y z first to find the maximum of y and z.
-- Then, it uses that result to find the maximum between x and the result of max y z.

-- max3 :: Int -> Int -> Int -> Int
-- max3 x y z = max x $ max y z  -- Using $

-- -- or equivalently

-- max3' :: Int -> Int -> Int -> Int
-- max3' x y z = max x (max y z) -- Using parentheses

-- main :: IO ()
-- main = do
--     print $ max3 1 5 3   -- Output: 5
--     print $ max3' 1 5 3  -- Output: 5



-- 3. Write a function f :: (Int -> String) -> (String -> Bool) -> (Int -> Bool), 
-- where the resulting function is the composition of the two input functions.
compositionFunction :: (Int -> String) -> (String -> Bool) -> (Int -> Bool)
compositionFunction ab bc = bc . ab 

-- compositionFunction takes two arguments:
-- ab, which is a function of type Int -> String.
-- bc, which is a function of type String -> Bool.
-- function ab bc = bc . ab creates a new function that:
-- Takes an Int.
-- Applies ab to convert it to a String.
-- Applies bc to the resulting String to produce a Bool.

-- compositionFunction :: (Int -> String) -> (String -> Bool) -> (Int -> Bool)
-- compositionFunction ab bc = \x -> bc (ab x)

-- In this rewritten form:
-- ab is applied to x first.
-- bc is then applied to the result of ab x.
-- The behavior here is identical to using bc . ab, but with a clearer visual indication of the order in which each function is applied.



-- 4. Write a function g :: (Int -> Bool) -> (Bool -> String) -> Int -> String, 
-- where the resulting string is the output of the second input function.
g :: (Int -> Bool) -> (Bool -> String) -> Int -> String
g f1 f2 x = f2 (f1 x)

-- First argument: g takes a function of type (Int -> Bool). 
-- This means it takes an integer and returns a boolean. Let's call this function f1.

-- Second argument: g takes another function of type (Bool -> String). 
-- This means it takes a boolean and returns a string. We'll call this function f2.

-- Third argument: g takes an integer as its third argument (denoted as Int).
-- Return type: The entire function g returns a string (String).

-- Input parameter x: The function takes an integer x as input.
-- Apply f1 to x:

-- The first step is to apply the function f1 (which has the type Int -> Bool) to the integer x.
-- This results in a boolean value, which we can denote as b = f1 x.
-- Essentially, f1 will evaluate whether x meets some condition, returning either True or False.

-- Apply f2 to the result of f1 x:
-- The next step is to apply the second function f2 (which has the type Bool -> String) to the boolean value returned by f1.
-- This results in a string, which we can denote as s = f2 (f1 x).

-- The function f2 will convert the boolean value into a string representation based on its internal logic.
-- Return the result: Finally, the function returns the string produced by f2.

-- can also be written as 
-- g :: (Int -> Bool) -> (Bool -> String) -> Int -> String
-- g f1 f2 x = f2 $ f1 x


--  Function to check if a number is positive
-- isPositive :: Int -> Bool
-- isPositive n = n > 0

-- -Function to convert a boolean to a string
-- boolToString :: Bool -> String
-- boolToString True = "The number is positive."
-- boolToString False = "The number is not positive."

--  Main usage of the function g
-- g :: (Int -> Bool) -> (Bool -> String) -> Int -> String
-- g f1 f2 x = f2 (f1 x)

--  Examples of using g
-- result1 :: String
-- result1 = g isPositive boolToString 5   -- Output: "The number is positive."

-- result2 :: String
-- result2 = g isPositive boolToString (-3) -- Output: "The number is not positive."

--  You can print the results in a main function if needed
-- main :: IO ()
-- main = do
--     putStrLn result1
--     putStrLn result2


-- 5. Write a function twice :: (Int -> Int) -> Int -> Int that applies the input function two times
-- to the input integer.
twice :: (Int -> Int) -> Int -> Int
twice f1 x = f1 (f1 x)

-- Input:
-- The function takes two parameters: a function f1 of type (Int -> Int) and an integer x.
-- Application:
-- It applies f1 to x, and then it applies f1 again to the result of the first application.
