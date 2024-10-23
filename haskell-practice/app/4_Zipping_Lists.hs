-- Zipping is a way of combining two lists into a list of pairs (tuples). 
-- The zip function takes two lists as inputs and combines corresponding elements from each list into pairs. 
-- The result is a list of tuples, with each tuple containing one element 
-- from each of the input lists at the same position.

-- The length of the resulting list is determined by the shorter of the two input lists. 
-- If one list is longer than the other, the extra elements are ignored, 
-- which ensures that every tuple has an element from each list.

-- Syntax of zip
-- zip :: [a] -> [b] -> [(a, b)]

-- zip takes two lists: [a] and [b].
-- It returns a list of pairs [(a, b)], where each pair contains one element from each input list.

-- Examples of zip
-- zip [1, 2, 3] ["One", "Two", "Three"] results in [(1, "One"), (2, "Two"), (3, "Three")].
-- zip [1, 2] [True, False, True] results in [(1, True), (2, False)]—note that the last True in the second list is ignored.

-- Other Related Functions
-- zipWith: Allows you to combine two lists with a custom function rather than just forming tuples. 
-- For example, zipWith (+) [1, 2, 3] [4, 5, 6] gives [5, 7, 9].

-- unzip: Splits a list of tuples into two separate lists. 
-- For example, unzip [(1, "One"), (2, "Two")] returns ([1, 2], ["One", "Two"]).


-- Basic Zipping of Two Lists of Integers
-- Pairs elements from two lists of integers.
zipIntsExample :: [(Int, Int)]
zipIntsExample = zip [1, 2, 3] [4, 5, 6] -- Result: [(1,4), (2,5), (3,6)]


-- Zipping Lists of Characters and Booleans
-- Combines a list of characters with a list of boolean values.
zipCharBoolExample :: [(Char, Bool)]
zipCharBoolExample = zip ['A', 'B', 'C'] [True, False, True] -- Result: [('A',True), ('B',False), ('C',True)]


-- Zipping with Different List Lengths
-- Zips lists where one list is shorter, ignoring extra elements in the longer list.
zipShortLongExample :: [(Int, String)]
zipShortLongExample = zip [1, 2] ["Hello", "World", "Haskell"] -- Result: [(1,"Hello"), (2,"World")]


-- Zipping with Strings and Integers
-- Combines a list of integers with a list of strings to label items.
zipLabelExample :: [(String, Int)]
zipLabelExample = zip ["First", "Second", "Third"] [10, 20, 30] -- Result: [("First",10), ("Second",20), ("Third",30)]


-- Zipping Lists to Form Coordinate Pairs
-- Forms coordinate pairs by zipping an x and y list together.
zipCoordinatesExample :: [(Int, Int)]
zipCoordinatesExample = zip [1, 2, 3] [4, 5, 6] -- Result: [(1,4), (2,5), (3,6)]


-- Creating a Dictionary-like Structure with Zipping
-- Combines a list of keys with a list of values.
zipDictExample :: [(String, Int)]
zipDictExample = zip ["One", "Two", "Three"] [1, 2, 3] -- Result: [("One",1), ("Two",2), ("Three",3)]


-- Zipping Three Lists Using zip3
-- Combines three lists into a list of triplets, where zip3 is specifically for zipping three lists.
zipThreeListsExample :: [(Int, Char, Bool)]
zipThreeListsExample = zip3 [1, 2, 3] ['a', 'b', 'c'] [True, False, True]
-- Result: [(1,'a',True), (2,'b',False), (3,'c',True)]


-- Using zipWith to Add Corresponding Elements of Two Lists
-- Uses zipWith and a custom function (+) to sum corresponding elements.
zipWithAddExample :: [Int]
zipWithAddExample = zipWith (+) [1, 2, 3] [10, 20, 30] -- Result: [11, 22, 33]


-- Zipping Lists of Lists to Form Matrix Row-Column Pairs
-- Creates pairs from a list of lists, treating the inner lists as matrix rows and columns.
zipMatrixExample :: [([Int], [Int])]
zipMatrixExample = zip [[1,2], [3,4]] [[5,6], [7,8]] -- Result: [([1,2],[5,6]), ([3,4],[7,8])]