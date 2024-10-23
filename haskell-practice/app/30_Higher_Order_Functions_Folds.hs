-- Folds are powerful functions that process a list to produce a single accumulated value.
-- There are two main types: left fold (foldl) and right fold (foldr).

-- 1. Left Fold (foldl)
-- Definition:
-- foldl takes three arguments:
-- - A function that takes two arguments: the accumulator and the current element of the list.
-- - An initial value for the accumulator.
-- - The list to be processed.

-- Example: foldl (+) 0 [1, 2, 3, 4]
-- Step-by-Step Breakdown:

-- Initial Setup:
-- Function: (+) (addition)
-- Initial Value: 0
-- List: [1, 2, 3, 4]

-- How It Works:
-- The fold will take each element from the list one by one and apply the function (+) to the 
-- current accumulator value and the current element from the list.

-- Steps of Execution:

-- 1. Start with Initial Value: 0
--    - Accumulator: 0
--    - Current List Element: 1
--    - Operation: 0 + 1
--    - New Accumulator Value: 1

-- 2. Next Element: 2
--    - Accumulator: 1
--    - Current List Element: 2
--    - Operation: 1 + 2
--    - New Accumulator Value: 3

-- 3. Next Element: 3
--    - Accumulator: 3
--    - Current List Element: 3
--    - Operation: 3 + 3
--    - New Accumulator Value: 6

-- 4. Next Element: 4
--    - Accumulator: 6
--    - Current List Element: 4
--    - Operation: 6 + 4
--    - New Accumulator Value: 10

-- Final Result:
-- After processing all the elements, the final value of the accumulator is 10.
-- foldl (+) 0 [1, 2, 3, 4] -- Result: 10

-- Visual Representation:
-- Initial:      0
--               |
--               +--- 1 → 1
--               |
--               +--- 2 → 3
--               |
--               +--- 3 → 6
--               |
--               +--- 4 → 10

-- 2. Right Fold (foldr)
-- Definition:
-- foldr takes three arguments:
-- - A function that takes two arguments: the current element and the accumulator.
-- - An initial value for the accumulator.
-- - The list to be processed.

-- Example: foldr (+) 0 [1, 2, 3, 4]
-- Step-by-Step Breakdown:

-- Initial Setup:
-- Function: (+) (addition)
-- Initial Value: 0
-- List: [1, 2, 3, 4]

-- How It Works:
-- The fold will process the list from the right (the end) to the left (the beginning).

-- Steps of Execution:

-- 1. Start with Initial Value: 0
--    - Current List Element: 4
--    - Operation: 4 + 0
--    - New Accumulator Value: 4

-- 2. Next Element: 3
--    - Accumulator: 4
--    - Current List Element: 3
--    - Operation: 3 + 4
--    - New Accumulator Value: 7

-- 3. Next Element: 2
--    - Accumulator: 7
--    - Current List Element: 2
--    - Operation: 2 + 7
--    - New Accumulator Value: 9

-- 4. Next Element: 1
--    - Accumulator: 9
--    - Current List Element: 1
--    - Operation: 1 + 9
--    - New Accumulator Value: 10

-- Final Result:
-- After processing all the elements from right to left, the final value of the accumulator is 10.
-- foldr (+) 0 [1, 2, 3, 4] -- Result: 10

-- Visual Representation:
-- Initial:      0
--               |
--               4 → 4
--               |
--               3 → 7
--               |
--               2 → 9
--               |
--               1 → 10

-- Comparison: foldl vs. foldr
-- - Direction: foldl processes from left to right, while foldr processes from right to left.
-- - Associativity: foldl is left-associative; it groups operations on the left. 
--   foldr is right-associative; it groups operations on the right.
-- - Performance: foldl can be more memory-efficient for long lists since it can build 
--   up a large stack with deeply nested operations. foldr can be less efficient in such cases.

-- Both folds can achieve the same result for certain operations (like addition), but 
-- their behavior may differ for non-associative operations (like subtraction or division).

module Main where

-- Left Fold Summing a List of Numbers
sumList :: [Int] -> Int
sumList = foldl (+) 0

-- Right Fold Creating a Concatenated String
concatString :: [String] -> String
concatString = foldr (++) ""

-- Left Fold Counting Elements in a List
countElements :: [a] -> Int
countElements = foldl (\acc _ -> acc + 1) 0

-- Right Fold Reversing a List
reverseList :: [a] -> [a]
reverseList = foldr (\x acc -> acc ++ [x]) []

-- Left Fold Building a List of Squares
squaresList :: [Int] -> [Int]
squaresList = foldl (\acc x -> acc ++ [x^2]) []

-- Right Fold to Check if Any Element Matches a Condition
anyMatch :: (a -> Bool) -> [a] -> Bool
anyMatch p = foldr ((||) . p) False


-- Left Fold to Flatten a List of Lists
flattenList :: [[a]] -> [a]
flattenList = foldl (++) []

-- Right Fold to Create a Nested Structure
nestedStructure :: [a] -> [[a]]
nestedStructure = foldr (\x acc -> [x] : acc) []

-- Left Fold to Calculate Factorial
factorial :: Int -> Int
factorial n = foldl (*) 1 [1..n]

-- Main function to run the examples
main :: IO ()
main = do
    -- Easy Examples
    print $ sumList [1, 2, 3, 4, 5] -- Result: 15
    print $ concatString ["Haskell ", "is ", "great!"] -- Result: "Haskell is great!"
    print $ countElements ["a", "b", "c"] -- Result: 3
    
    -- Medium Examples
    print $ reverseList [1, 2, 3, 4, 5] -- Result: [5, 4, 3, 2, 1]
    print $ squaresList [1, 2, 3] -- Result: [1, 4, 9]
    print $ anyMatch odd [2, 4, 6, 7] -- Result: True
    
    -- Hard Examples
    print $ flattenList [[1, 2], [3, 4], [5]] -- Result: [1, 2, 3, 4, 5]
    print $ nestedStructure [1, 2, 3] -- Result: [[1], [2], [3]]
    print $ factorial 5 -- Result: 120