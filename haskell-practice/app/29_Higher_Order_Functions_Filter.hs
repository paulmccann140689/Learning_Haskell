-- Filter is a higher-order function that allows you to create a new list by selecting only 
-- those elements of an original list that satisfy a specific condition, often called a "predicate." 
-- This function is frequently used for tasks like extracting specific values from a list, 
-- removing elements that don't meet criteria, and refining data collections.

-- Type Signature of filter
-- filter :: (a -> Bool) -> [a] -> [a]

-- The type signature indicates:
-- filter takes two arguments:
-- A function of type (a -> Bool), which is the predicate function. 
-- This function takes an element of type a and returns True if the element should be kept in the list, and False if it should be excluded.
-- A list of elements of type [a].
-- It returns a new list [a] containing only the elements that satisfy the predicate.

-- How filter Works
-- Apply Predicate: filter iterates through each element in the list.
-- Evaluate Condition: For each element, filter applies the predicate function to decide if it should include or exclude that element.
-- Build New List: Only elements for which the predicate returns True are included in the resulting list.

-- Basic Example
-- filter even [1, 2, 3, 4, 5, 6] -- Result: [2, 4, 6]

-- filter applies even to each element in [1, 2, 3, 4, 5, 6].
-- For 1, even 1 returns False, so 1 is excluded.
-- For 2, even 2 returns True, so 2 is included.
-- This process continues for each element, resulting in [2, 4, 6], a list of even numbers.


-- Filtering Positive Numbers

-- filterPositive :: [Int] -> [Int]
-- filterPositive xs = filter (> 0) xs
-- The function (> 0) is the predicate, which checks if a number is greater than zero.
-- Only elements that are greater than zero will be included in the resulting list.

import Data.Char (toLower)

-- Filtering even numbers
filterEven :: [Int] -> [Int]
filterEven xs = filter even xs
main :: IO ()
main = print (filterEven [1, 2, 3, 4, 5, 6]) -- Expected output: [2, 4, 6]


-- Filtering positive numbers
filterPositive :: [Int] -> [Int]
filterPositive xs = filter (> 0) xs
main2 :: IO ()
main2 = print (filterPositive [-3, -1, 0, 1, 3]) -- Expected output: [1, 3]


-- Filtering non-empty strings
filterNonEmpty :: [String] -> [String]
filterNonEmpty xs = filter (not . null) xs
main3 :: IO ()
main3 = print (filterNonEmpty ["hello", "", "world", "", "Haskell"]) -- Expected output: ["hello", "world", "Haskell"]


-- Filtering strings with length greater than 3
filterLongStrings :: [String] -> [String]
filterLongStrings xs = filter (\s -> length s > 3) xs
main4 :: IO ()
main4 = print (filterLongStrings ["hi", "hello", "ok", "world"]) -- Expected output: ["hello", "world"]


-- Filtering numbers that are divisible by 3
filterDivisibleByThree :: [Int] -> [Int]
filterDivisibleByThree xs = filter (\x -> x `mod` 3 == 0) xs
main5 :: IO ()
main5 = print (filterDivisibleByThree [1, 3, 4, 6, 7, 9]) -- Expected output: [3, 6, 9]


-- Filtering out strings containing the letter 'a'
filterNoA :: [String] -> [String]
filterNoA xs = filter (notElem 'a') xs
main6 :: IO ()
main6 = print (filterNoA ["apple", "banana", "cherry", "blueberry"]) -- Expected output: ["cherry", "blueberry"]


-- Filtering lists of integers where all elements are positive
filterAllPositive :: [[Int]] -> [[Int]]
filterAllPositive xs = filter (all (> 0)) xs
main7 :: IO ()
main7 = print (filterAllPositive [[1, 2, 3], [-1, 2, 3], [4, 5, 6]]) -- Expected output: [[1, 2, 3], [4, 5, 6]]


-- Filtering strings that start with a vowel
startsWithVowel :: String -> Bool
startsWithVowel [] = False
startsWithVowel (x:_) = toLower x `elem` "aeiou"
filterVowelStart :: [String] -> [String]
filterVowelStart xs = filter startsWithVowel xs
main8 :: IO ()
main8 = print (filterVowelStart ["apple", "banana", "orange", "pear"]) -- Expected output: ["apple", "orange"]


-- Filtering lists of integers that contain at least one odd number
filterContainsOdd :: [[Int]] -> [[Int]]
filterContainsOdd xs = filter (any odd) xs
main9 :: IO ()
main9 = print (filterContainsOdd [[1, 2, 3], [2, 4, 6], [7, 8, 10]]) -- Expected output: [[1, 2, 3]