-- Currying
-- Currying refers to the process of transforming a function that takes multiple arguments into a series of functions 
-- that each take a single argument. In Haskell, all functions are curried by default. 
-- This means that a function that appears to take multiple arguments is actually a chain of single-argument functions.

-- For example, a function add x y = x + y is treated as:

-- First, a function that takes x and returns a new function.
-- This new function takes y and returns the result of x + y.
-- Thus, add 5 3 is really two steps:

-- add 5 gives a new function \y -> 5 + y.
-- Then applying 3 to that new function gives 8.
-- Partial Application
-- Partial application is a direct consequence of currying. It allows you to "partially apply" a function by fixing some of its arguments 
-- and returning a new function that takes the remaining arguments.

-- For example, if you have a function multiply x y = x * y, you can partially apply it by fixing one of its arguments. 
--     For instance, applying 2 to multiply gives you a new function that multiplies its argument by 2. 
--     This partial application can be used to create specialized versions of more general functions.

-- Key Differences Between Currying and Partial Application
-- Currying: The process of transforming a function with multiple arguments into a series of functions with single arguments. A
-- ll Haskell functions are curried by default.

-- Partial Application: The act of applying some (but not all) arguments to a curried function, 
-- producing a new function that takes the remaining arguments.


-- Curried Addition with Partial Application:
add :: Int -> Int -> Int
add x y = x + y

addTen :: Int -> Int
addTen = add 10

addExample1 :: Int
addExample1 = addTen 5
-- Usage: addExample1  -- Result: 15
-- Here, add is a curried function, and addTen is created by partially applying 10 to add. 
-- Now addTen is a function that adds 10 to its argument.




-- Curried Subtraction:
subtractCurried :: Int -> Int -> Int
subtractCurried x y = x - y

subtractFive :: Int -> Int
subtractFive = subtractCurried 5

subtractExample1 :: Int
subtractExample1 = subtractFive 3
-- Usage: subtractExample1  -- Result: 2
-- We create a curried subtraction function and then partially apply 5 to create subtractFive.





-- Curried Multiplication:
multiply :: Int -> Int -> Int
multiply x y = x * y

multiplyBy3 :: Int -> Int
multiplyBy3 = multiply 3

multiplyExample1 :: Int
multiplyExample1 = multiplyBy3 4
-- Usage: multiplyExample1  -- Result: 12
-- This is a simple curried multiplication example. We partially apply 3 to multiply, creating a multiplyBy3 function.





-- Curried Power Function:
power :: Int -> Int -> Int
power base exponent = base ^ exponent

square :: Int -> Int
square = power 2

powerExample1 :: Int
powerExample1 = square 5
-- Usage: powerExample1  -- Result: 32
-- We partially apply 2 to power, creating a square function, which computes powers of 2.





-- Curried Division:
divide :: Double -> Double -> Double
divide x y = x / y

divideBy2 :: Double -> Double
divideBy2 = divide 2

divideExample1 :: Double
divideExample1 = divideBy2 8
-- Usage: divideExample1  -- Result: 0.25
-- We partially apply 2 to the divide function to create a new function, divideBy2, which divides by 2.





-- Curried String Concatenation:
concatenate :: String -> String -> String
concatenate x y = x ++ y

greet :: String -> String
greet = concatenate "Hello, "

concatExample1 :: String
concatExample1 = greet "Alice"
-- Usage: concatExample1  -- Result: "Hello, Alice"
-- We partially apply "Hello, " to concatenate to create a greeting function.






-- Curried Function for Mapping Over a List:
mapCurried :: (a -> b) -> [a] -> [b]
mapCurried f xs = map f xs

incrementList :: [Int] -> [Int]
incrementList = mapCurried (+1)

mapExample1 :: [Int]
mapExample1 = incrementList [1, 2, 3]
-- Usage: mapExample1  -- Result: [2, 3, 4]
-- We use a curried mapCurried function to apply (+1) to each element in a list, creating a new function incrementList.





-- Curried Function for Folding a List:
foldlCurried :: (b -> a -> b) -> b -> [a] -> b
foldlCurried f acc xs = foldl f acc xs

sumList :: [Int] -> Int
sumList = foldlCurried (+) 0

foldExample1 :: Int
foldExample1 = sumList [1, 2, 3, 4, 5]
-- Usage: foldExample1  -- Result: 15
-- This curried foldl function creates sumList, a function that sums a list of integers.





-- Curried Function for Filtering a List:
filterCurried :: (a -> Bool) -> [a] -> [a]
filterCurried f xs = filter f xs

evens :: [Int] -> [Int]
evens = filterCurried even

filterExample1 :: [Int]
filterExample1 = evens [1, 2, 3, 4, 5, 6]
-- Usage: filterExample1  -- Result: [2, 4, 6]
-- This curried filterCurried function is partially applied with even to create evens, which filters out odd numbers from a list.