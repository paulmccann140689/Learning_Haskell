-- In Haskell, lists are a fundamental data structure, and there are various functions and techniques 
-- for accessing elements within them. Accessing elements can range from retrieving a single item (like the head of the list) 
-- to slicing or extracting portions of the list. 
-- These functions can be categorized based on what they return—either a single element or a sublist. 
-- Since lists in Haskell are immutable and lazy, these operations don’t modify the original list 
-- but create new versions or references as needed.

-- Common Functions for Accessing Lists
-- head: Retrieves the first element of a list. This function throws an error if the list is empty.
-- Example: head [1, 2, 3] returns 1.

-- tail: Returns all elements of a list except the first one. This also throws an error if the list is empty.
-- Example: tail [1, 2, 3] returns [2, 3].

-- last: Retrieves the last element in the list. Like head, it will throw an error if the list is empty.
-- Example: last [1, 2, 3] returns 3.

-- init: Returns all elements of a list except the last one. Throws an error if the list is empty.
-- Example: init [1, 2, 3] returns [1, 2].

-- !! (Indexing): Accesses an element by its zero-based index in the list.
-- Example: [1, 2, 3] !! 1 returns 2.

-- take: Extracts a specified number of elements from the start of the list.
-- Example: take 2 [1, 2, 3] returns [1, 2].

-- drop: Removes a specified number of elements from the start of the list.
-- Example: drop 2 [1, 2, 3] returns [3].

-- length: Returns the number of elements in the list.
-- Example: length [1, 2, 3] returns 3.

-- null: Checks if a list is empty, returning True if it is and False otherwise.
-- Example: null [] returns True.


-- Getting the First Element Using head
-- Accesses the first element of a simple list.
headExample :: Int
headExample = head [10, 20, 30] -- Result: 10



-- Getting All Elements Except the First Using tail
-- Retrieves all elements except the first in the list.
tailExample :: [Int]
tailExample = tail [10, 20, 30] -- Result: [20, 30]



-- Accessing an Element by Index Using !!
-- Retrieves the second element (index 1) from the list.
indexExample :: Int
indexExample = [10, 20, 30] !! 1 -- Result: 20



-- Getting the Last Element Using last
-- Retrieves the last element of a list.
lastExample :: Int
lastExample = last [5, 10, 15, 20] -- Result: 20



-- Taking the First Three Elements Using take
-- Extracts the first three elements from a longer list.
takeExample :: [Int]
takeExample = take 3 [10, 20, 30, 40, 50] -- Result: [10, 20, 30]



-- Dropping the First Two Elements Using drop
-- Removes the first two elements from a list, keeping the rest.
dropExample :: [Int]
dropExample = drop 2 [10, 20, 30, 40, 50] -- Result: [30, 40, 50]



-- Combining take and drop for List Slicing
-- This example extracts a sublist by first dropping the initial elements, then taking a specific number of elements.
sliceExample :: [Int]
sliceExample = take 3 (drop 1 [10, 20, 30, 40, 50]) -- Result: [20, 30, 40]



-- Using init and tail to Get Middle Elements
-- Removes both the first and last elements from the list to get only the middle section.
middleElementsExample :: [Int]
middleElementsExample = init (tail [10, 20, 30, 40, 50]) -- Result: [20, 30, 40]



-- Checking for Empty and Non-Empty Lists Using null and length
-- Combines null and length to perform checks on a list, returning different messages based on whether the list has elements.
checkListExample :: String
checkListExample = if null [1, 2, 3]
                   then "The list is empty."
                   else "The list has " ++ show (length [1, 2, 3]) ++ " elements."
-- Result: "The list has 3 elements."