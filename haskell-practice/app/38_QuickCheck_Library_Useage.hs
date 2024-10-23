-- QuickCheck is a Haskell library for automatic testing of program properties. 
-- It allows you to specify properties that your functions should satisfy 
-- and then automatically generates test cases to check those properties.

module Main where

import Test.QuickCheck
import Data.List (sort)

-- Easy Examples

-- Property: Length of a List
prop_len :: Int -> Bool
prop_len n = if n >= 0 then length [1..n] == n else True

-- Property: Reversing a List Twice
prop_reverseTwice :: [Int] -> Bool
prop_reverseTwice xs = reverse (reverse xs) == xs

-- Property: Append List Lengths
prop_appendLength :: [Int] -> [Int] -> Bool
prop_appendLength xs ys = length (xs ++ ys) == length xs + length ys

-- Medium Examples

-- Property: Sorting a List
prop_sorted :: [Int] -> Bool
prop_sorted xs = sort xs == sort (xs :: [Int])

-- Property: Sum of Reversed Lists
prop_sumReverse :: [Int] -> Bool
prop_sumReverse xs = sum xs == sum (reverse xs)

-- Property: Idempotent Function
prop_takeIdempotent :: [Int] -> Bool
prop_takeIdempotent xs = take 3 (take 3 xs) == take 3 xs

-- Hard Examples

-- Property: Distribution of Even and Odd Numbers
prop_evenOddCount :: [Int] -> Bool
prop_evenOddCount xs = countEvens xs + countOdds xs == length xs
  where
    countEvens = length . filter even
    countOdds = length . filter odd

-- Property: Associativity of List Concatenation
prop_concatAssociativity :: [Int] -> [Int] -> [Int] -> Bool
prop_concatAssociativity xs ys zs = (xs ++ ys) ++ zs == xs ++ (ys ++ zs)

-- Property: Nested List Flattening
prop_flattenCount :: [[Int]] -> Bool
prop_flattenCount xss = length (concat xss) == sum (map length xss)

-- Main function to run the properties
main :: IO ()
main = do
    quickCheck prop_len
    quickCheck prop_reverseTwice
    quickCheck prop_appendLength
    quickCheck prop_sorted
    quickCheck prop_sumReverse
    quickCheck prop_takeIdempotent
    quickCheck prop_evenOddCount
    quickCheck prop_concatAssociativity
    quickCheck prop_flattenCount
