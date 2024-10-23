-- In Haskell, map is a higher-order function that applies a function to each element in a list, 
-- transforming the list by applying that function individually to every element. 

-- The type signature for map is:
-- map :: (a -> b) -> [a] -> [b]
-- This means that map takes a function (a -> b) (a function that maps an element of type a to type b) and a list of type [a], returning a list of type [b].

-- Explanation of map:
-- Input Function: The first argument to map is a function that specifies how to transform each element.
-- List Argument: The second argument is a list of elements on which to apply the function.
-- Transformation: For each element in the list, map applies the given function and returns a new list with each transformed element.
-- For instance, with map (+1) [1, 2, 3], the function (+1) is applied to each element, resulting in [2, 3, 4].

import Data.Char (toUpper)

-- Adding a constant to each element
addOneToList :: [Int] -> [Int]
addOneToList = map (+1)
main :: IO ()
main = print (addOneToList [1, 2, 3]) -- Expected output: [2, 3, 4]
-- This example adds 1 to each element in the list [1, 2, 3].


-- Doubling each element
doubleEachElement :: [Int] -> [Int]
doubleEachElement = map (*2)
main2 :: IO ()
main2 = print (doubleEachElement [1, 2, 3]) -- Expected output: [2, 4, 6]
-- Here, each element in [1, 2, 3] is doubled.


-- Converting boolean values to strings
boolToString :: [Bool] -> [String]
boolToString = map show
main3 :: IO ()
main3 = print (boolToString [True, False, True]) -- Expected output: ["True", "False", "True"]
-- The show function converts each boolean in [True, False, True] to a string.


-- Negating each number in a list
negateList :: [Int] -> [Int]
negateList = map negate
main4 :: IO ()
main4 = print (negateList [1, -2, 3, -4]) -- Expected output: [-1, 2, -3, 4]
-- Each element is negated (positive to negative, and vice versa).


-- Capitalizing the first character of each string
capitalizeFirst :: [String] -> [String]
capitalizeFirst xs = map (\(x:xs) -> toUpper x : xs) xs
main5 :: IO ()
main5 = print (capitalizeFirst ["hello", "world"]) -- Expected output: ["Hello", "World"]
-- This function capitalizes the first character of each word in the list ["hello", "world"].


-- Adding "!" to each word in a list
addExclamation :: [String] -> [String]
addExclamation xs = map (++ "!") xs
main6 :: IO ()
main6 = print (addExclamation ["Hello", "World"]) -- Expected output: ["Hello!", "World!"]
-- Each string in the list has "!" appended to it.


-- Applying multiple transformations on numbers
transformNumbers :: [Int] -> [Int]
transformNumbers xs = map (\x -> if even x then x * 2 else x + 1) xs
main7 :: IO ()
main7 = print (transformNumbers [1, 2, 3, 4]) -- Expected output: [2, 4, 4, 8]
-- Here, each element is doubled if it's even; otherwise, it has 1 added to it.


-- Transforming a list of tuples
swapTuples :: [(a, b)] -> [(b, a)]
swapTuples xs = map (\(x, y) -> (y, x)) xs
main8 :: IO ()
main8 = print (swapTuples [(1, 'a'), (2, 'b'), (3, 'c')]) -- Expected output: [('a', 1), ('b', 2), ('c', 3)]
-- Each tuple in the list [(1, 'a'), (2, 'b'), (3, 'c')] is transformed by swapping the elements of the tuple.


-- Filtering and transforming a list in one pass
filterAndTransform :: [[Int]] -> [[Int]]
filterAndTransform xs = map (filter even . map (*2)) xs
main9 :: IO ()
main9 = print (filterAndTransform [[1, 2], [3, 4, 5], [6, 7, 8]]) -- Expected output: [[4], [8], [12, 16]]
-- Each list in [[1, 2], [3, 4, 5], [6, 7, 8]] is transformed by doubling each element and keeping only the even results.