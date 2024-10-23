-- In Haskell, a list is a data structure that stores an ordered sequence of elements, all of which must be of the same type. 
-- Lists are one of the most commonly used data structures in functional programming due to their simplicity, flexibility, 
-- and the wide range of functions available to manipulate them.

-- Lists in Haskell are denoted by square brackets ([ ]). For example, [1, 2, 3] is a list of integers, 
-- and ["Hello", "World"] is a list of strings. Lists in Haskell are immutable, meaning once a list is created, 
-- it cannot be altered directly. However, we can create new lists by performing operations on existing ones.

-- Key Properties of Lists
-- Homogeneous Elements: Every element in a list must be of the same type. 
-- For example, [1, 2, "Hello"] is invalid because it mixes Int and String types.

-- Lazy Evaluation: Lists in Haskell are lazily evaluated, meaning elements are only computed when needed. 
-- This allows us to work with infinite lists, like [1..], without causing a program to crash.

-- Recursive Structure: Lists are inherently recursive, meaning they can be defined in terms of themselves, 
-- often using the : (cons) operator to construct lists element by element.

-- Basic Operations on Lists
-- Head: head [1, 2, 3] returns the first element, 1.
-- Tail: tail [1, 2, 3] returns all elements except the first, [2, 3].
-- Cons (:) Operator: 1 : [2, 3] adds 1 to the front, resulting in [1, 2, 3].
-- Concatenation (++): [1, 2] ++ [3, 4] combines two lists, resulting in [1, 2, 3, 4].
-- Length: length [1, 2, 3] gives the number of elements, 3.
-- Indexing: Lists support indexing with the !! operator, e.g., [1, 2, 3] !! 1 results in 2.

-- exampleList1 :: [Int]
-- exampleList1 = [1, 2, 3]

-- exampleList2 :: [String]
-- exampleList2 = ["Hello", "FP"]

-- exampleList1 is a list of integers.
-- exampleList2 is a list of strings.


-- List of Characters
-- A list of characters (also known as a string in Haskell).
charList :: [Char]
charList = ['H', 'a', 's', 'k', 'e', 'l', 'l']



-- Range of Numbers
-- Haskell provides an easy way to define a list of numbers using a range.
numberRange :: [Int]
numberRange = [1..5] -- Result: [1, 2, 3, 4, 5]



-- List Concatenation
-- Using ++ to combine two lists.
concatList :: [Int]
concatList = [1, 2] ++ [3, 4] -- Result: [1, 2, 3, 4]



-- List Comprehension with Filtering
-- Using list comprehension to create a list of even numbers up to 10.
evenNumbers :: [Int]
evenNumbers = [x | x <- [1..10], even x] -- Result: [2, 4, 6, 8, 10]



-- Mapping a Function Over a List
-- Using map to square each element in a list.
squaredList :: [Int]
squaredList = map (^2) [1, 2, 3, 4] -- Result: [1, 4, 9, 16]



-- List of Tuples
-- Creating a list of pairs using zip.
pairedList :: [(Int, Char)]
pairedList = zip [1, 2, 3] ['a', 'b', 'c'] -- Result: [(1, 'a'), (2, 'b'), (3, 'c')]



-- Filtering and Mapping Together
-- Filtering a list of numbers for even values and then doubling them.
filteredDoubledList :: [Int]
filteredDoubledList = map (*2) (filter even [1..10]) -- Result: [4, 8, 12, 16, 20]



-- Using foldr to Sum Elements in a List
-- Folding a list to sum all its elements.
sumList :: [Int] -> Int
sumList xs = foldr (+) 0 xs
sumExample :: Int
sumExample = sumList [1, 2, 3, 4] -- Result: 10



-- Infinite List with Take
-- Creating an infinite list of multiples of 3 and taking the first 10 elements.
multiplesOf3 :: [Int]
multiplesOf3 = take 10 [x * 3 | x <- [1..]] -- Result: [3, 6, 9, 12, 15, 18, 21, 24, 27, 30]