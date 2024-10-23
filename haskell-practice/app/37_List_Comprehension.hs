-- In Haskell, list comprehension is a powerful and expressive way to construct lists. 
-- It allows you to generate lists by specifying patterns, conditions, 
-- and transformations concisely within square brackets. 
-- Inspired by mathematical notation, list comprehension in Haskell enables you to filter, 
-- transform, and combine lists with minimal syntax, which is especially helpful 
-- for complex data transformations.

-- Basic Syntax of List Comprehension
-- The syntax for list comprehension is:

-- [expression | element <- list, condition]

-- Expression: Defines the transformation or computation applied to each element. 
-- This is what will appear in the resulting list.
-- Element <- List: Iterates through each element in the provided list, 
-- binding each element to a variable.
-- Condition(s): Specifies any constraints or filters to include only certain 
-- elements that meet specific criteria.


-- Double Each Number
-- This list comprehension doubles each number in a given range from 1 to 5.
doubleNumbers :: [Int]
doubleNumbers = [2 * x | x <- [1..5]] -- Result: [2, 4, 6, 8, 10]



-- Select Odd Numbers
-- This list comprehension filters out odd numbers from a range of numbers from 1 to 10.
oddNumbers :: [Int]
oddNumbers = [x | x <- [1..10], x `mod` 2 /= 0] -- Result: [1, 3, 5, 7, 9]



-- Pairing Elements with Filter
-- Creates pairs (x, y) from two lists such that the sum of x and y is greater than 5.
pairWithCondition :: [(Int, Int)]
pairWithCondition = [(x, y) | x <- [1, 2, 3], y <- [3, 4, 5], x + y > 5]
-- Result: [(1,5), (2,4), (2,5), (3,3), (3,4), (3,5)]



-- Multiplying Two Lists
-- Generates a list of products for each combination of elements from two lists.
multiplyLists :: [Int]
multiplyLists = [x * y | x <- [1, 2, 3], y <- [10, 20]] -- Result: [10, 20, 20, 40, 30, 60]



-- Nested Comprehensions for Cartesian Product
-- Generates all pairs (x, y) where x is from 1 to 3, and y is from 4 to 6, filtering pairs where both are odd.
cartesianProductOdd :: [(Int, Int)]
cartesianProductOdd = [(x, y) | x <- [1..3], y <- [4..6], x `mod` 2 == 1, y `mod` 2 == 1]
-- Result: [(1,5), (3,5)]



-- Prime Numbers within a Range
-- Generates a list of prime numbers up to 20 using a list comprehension with filtering.
isPrime :: Int -> Bool
isPrime n = null [x | x <- [2..n - 1], n `mod` x == 0]

primesUpTo20 :: [Int]
primesUpTo20 = [x | x <- [2..20], isPrime x] -- Result: [2, 3, 5, 7, 11, 13, 17, 19]


-- Question 1: Double the Elements
-- Write a function that takes a list of integers and returns 
-- a new list with each element doubled using list comprehension.

double :: [Int] -> [Int]
double xs = [2 * x | x <- xs]
result = double [1..10]

-- Question 2: Filter Even Numbers
-- Create a function that filters out even numbers 
-- from a list of integers using list comprehension.

filterEven :: [Int] -> [Int]
filterEven xs = [x | x <- xs, x `mod` 2 == 0] 
result2 = filterEven [1..10]

-- Question 3: Squares of Odd Numbers
-- Write a function that takes a list of integers and returns a new list 
-- containing the squares of the odd numbers from the original list using list comprehension.

squaresOdd :: [Int] -> [Int]
squaresOdd xs = [x * x | x <- xs, x `mod` 2 /= 0] 
result3 = squaresOdd [1..10]

-- Question 4: Length of Each Word
-- Given a list of strings, write a function that returns a 
-- list of the lengths of each string using list comprehension.

lengthWord :: [String] -> [Int]
lengthWord xs = [length x | x <- xs]
result4 = lengthWord ["hello", "World", "Haskell", "People"]

-- Question 5: Cartesian Product
-- Write a function that takes two lists and returns a list of 
-- tuples representing the Cartesian product of the two lists using list comprehension.