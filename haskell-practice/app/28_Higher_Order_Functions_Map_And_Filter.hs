module Main where

import Data.Char (toUpper)

-- Easy Examples

-- Doubling the even numbers in a list
doubleEvenNumbers :: [Int] -> [Int]
doubleEvenNumbers xs = map (*2) (filter even xs)

-- Uppercasing the non-empty strings
uppercaseNonEmpty :: [String] -> [String]
uppercaseNonEmpty xs = map (map toUpper) (filter (not . null) xs)

-- Adding 1 to all odd numbers
addOneToOddNumbers :: [Int] -> [Int]
addOneToOddNumbers xs = map (+1) (filter odd xs)

-- Medium Examples

-- Filtering even numbers and then squaring them
squareEvenNumbers :: [Int] -> [Int]
squareEvenNumbers xs = map (^2) (filter even xs)

-- Filtering out strings longer than 3 characters and reversing them
reverseShortStrings :: [String] -> [String]
reverseShortStrings xs = map reverse (filter (\s -> length s <= 3) xs)

-- Filtering numbers less than 5 and then halving the remaining numbers
halveNumbersAboveFive :: [Int] -> [Int]
halveNumbersAboveFive xs = map (`div` 2) (filter (>= 5) xs)

-- Hard Examples

-- Filtering lists of integers that contain at least one odd number and doubling those numbers
doubleOddInNestedLists :: [[Int]] -> [[Int]]
doubleOddInNestedLists xs = map (map (*2)) (filter (any odd) xs)

-- Filtering tuples by the second element and adding the first elements
sumFirstsOfFilteredTuples :: [(Int, Int)] -> Int
sumFirstsOfFilteredTuples xs = sum (map fst (filter (\(_, b) -> b > 0) xs))


-- Main function to run the examples
main :: IO ()
main = do
    -- Easy Examples
    print $ doubleEvenNumbers [1, 2, 3, 4, 5, 6] -- Result: [4, 8, 12]
    print $ uppercaseNonEmpty ["hello", "", "world"] -- Result: ["HELLO", "WORLD"]
    print $ addOneToOddNumbers [1, 2, 3, 4, 5] -- Result: [2, 4, 6]
    
    -- Medium Examples
    print $ squareEvenNumbers [1, 2, 3, 4, 5, 6] -- Result: [4, 16, 36]
    print $ reverseShortStrings ["hi", "hello", "ok", "bye"] -- Result: ["ih", "ko", "ey"]
    print $ halveNumbersAboveFive [3, 6, 7, 4, 5, 10] -- Result: [3, 3, 5]
    
    -- Hard Examples
    print $ doubleOddInNestedLists [[1, 2, 3], [2, 4, 6], [7, 8, 10]] -- Result: [[2, 4, 6], [14, 16, 20]]
    print $ sumFirstsOfFilteredTuples [(1, 2), (3, -1), (5, 4), (2, 0)] -- Result: 6
  
