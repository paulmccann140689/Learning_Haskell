-- The factorial function is a classic example of recursion in programming. 
-- In Haskell, recursion is particularly suited for this, as it allows you to define the 
-- factorial in terms of itself, breaking down the computation step-by-step until reaching a base case.

-- Explanation of Factorial with Recursion
-- The factorial of a non-negative integer n, written as n!, 
-- is defined as the product of all positive integers up to n!=n×(n−1)×(n−2)×⋯×1

-- In recursive terms, we can define the factorial function with:

-- Base Case: When n=0, the factorial of 0 is defined to be 1 (i.e., 0!=1).
-- Recursive Case: For any positive integer n, n!=n×(n−1)!.

-- fac :: Int -> Int
-- fac 0 = 1
-- fac n
--     | n < 0 = error "Factorial is undefined for negative numbers"
--     | otherwise = n * fac (n - 1)

-- Function Purpose: fac calculates the factorial of a non-negative integer.
-- Base Case: fac 0 = 1 — This stops the recursion when n reaches 0.
-- Error Handling: If n<0, it produces an error, since factorial is undefined for negative numbers.
-- Recursive Case: When n>0, it calculates n * fac (n - 1), reducing n by 1 in each recursive call until it reaches 0.

-- Power of a Number
power :: Int -> Int -> Int
power _ 0 = 1
power x n
    | n < 0     = error "Exponent should be non-negative"
    | otherwise = x * power x (n - 1)
-- Purpose: Computes x^n for a non-negative integer n.
-- Base Case: power _ 0 = 1 — Any number raised to the power of 0 is 1.
-- Recursive Case: x * power x (n - 1) — Multiplies x by itself n times.


-- Sum of First n Natural Numbers
sumN :: Int -> Int
sumN 0 = 0
sumN n
    | n < 0     = error "Sum is undefined for negative numbers"
    | otherwise = n + sumN (n - 1)
-- Purpose: Finds the sum of the first n natural numbers.
-- Base Case: sumN 0 = 0 — The sum of 0 is 0.
-- Recursive Case: n + sumN (n - 1) — Adds n to the sum of numbers up to n-1.


-- Product of List Elements
listProduct :: [Int] -> Int
listProduct [] = 1
listProduct (x:xs) = x * listProduct xs
-- Purpose: Calculates the product of all elements in a list.
-- Base Case: listProduct [] = 1 — An empty list has a product of 1.
-- Recursive Case: x * listProduct xs — Multiplies the head x by the product of the tail xs.


-- Greatest Common Divisor (Euclidean Algorithm)
gcdRec :: Int -> Int -> Int
gcdRec a 0 = abs a
gcdRec a b = gcdRec b (a `mod` b)
-- Purpose: Finds the greatest common divisor of two integers.
-- Base Case: gcdRec a 0 = abs a — When the second number is 0, return the absolute value of a.
-- Recursive Case: gcdRec b (a mod b) — Calls itself with the remainder until one number becomes 0.


-- Binary Representation of a Number
binary :: Int -> String
binary 0 = "0"
binary 1 = "1"
binary n
    | n < 0     = error "Binary representation is only for non-negative numbers"
    | otherwise = binary (n `div` 2) ++ show (n `mod` 2)
-- Purpose: Converts a non-negative integer to its binary string representation.
-- Base Cases: binary 0 = "0" and binary 1 = "1".
-- Recursive Case: binary (n div2) ++ show (nmod 2) — Recursively divides by 2, appending remainders.


-- Sum of Digits
sumDigits :: Int -> Int
sumDigits n
    | n < 10    = n
    | otherwise = (n `mod` 10) + sumDigits (n `div` 10)
-- Purpose: Computes the sum of the digits of a non-negative integer.
-- Base Case: If n < 10, return n (single-digit number).
-- Recursive Case: (n mod10) + sumDigits (ndiv 10) — Adds last digit to the sum of remaining digits.


-- Calculate the nth Fibonacci Number (Optimized with Accumulators)
fib :: Int -> Int
fib n = fibAux n 0 1
  where
    fibAux 0 a _ = a
    fibAux n a b = fibAux (n - 1) b (a + b)
-- Purpose: Computes the nth Fibonacci number.
-- Base Case: When n = 0, returns a (accumulator).
-- Recursive Case: Calls itself with updated accumulators for a and b.


-- Flatten Nested Lists
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concatMap flatten xs
-- Purpose: Flattens a nested list structure.
-- Base Case: When reaching an Elem, returns a single-element list.
-- Recursive Case: Uses concatMap to recursively flatten each nested list.


-- Compute Permutations of a List
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [x:ps | x <- xs, ps <- permutations (delete x xs)]
    where
        delete _ [] = []
        delete y (z:zs)
            | y == z    = zs
            | otherwise = z : delete y zs
-- Purpose: Generates all permutations of a list.
-- Base Case: permutations [] = [[]] — Only one permutation of an empty list.
-- Recursive Case: Picks each element x, computes permutations of the remaining elements, and concatenates x to each.