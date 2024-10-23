-- TUPLES

-- A tuple in Haskell is a fixed-size collection of elements, where each element can have a different type. 
-- Tuples are a simple way to group multiple values together, and their types are determined by the number of 
-- elements they contain and the types of those elements. 
-- Unlike lists, which are homogeneous (all elements must have the same type), 
-- Tuples can be heterogeneous hold elements of different types, and each position in the tuple has a specific type.

-- A tuple is represented by parentheses '()' enclosing the elements, with the elements separated by commas.
-- Each tuple has a specific arity, which is the number of elements in the tuple. 
-- The type of a tuple depends on the number and types of its elements.
-- You cannot add or remove elements from a tuple once it is created.

-- In summary, tuples are suitable for small collections of heterogeneously typed values, 
-- where the positions of the elements matter, but you don't need to name each element like in a record. 
-- They are often used for lightweight grouping of values, 
-- whereas lists and other algebraic data types are better suited for more complex and flexible data structures.

-- Tuple Examples

-- Simple 2-tuple with integers:
example1 :: (Int, Int)
example1 = (10, 20)

-- 2-tuple with a string and a float:
example2 :: (String, Float)
example2 = ("Pi", 3.14)

example3 :: (Int, Int, String)
example3 = (1, 2, "hello")

-- 2-tuple with a float and an integer:
example4 :: (Float, Int)
example4 = (1.0, 1)

-- Empty tuple (unit type):
example5 :: ()
example5 = ()

-- 3-tuple with a boolean, a character and an integer:
example6 :: (Bool, Char, Int)
example6 = (True, 'c', 42)

-- 3-tuple with an integer, boolean, and a list:
example7 :: (Int, Bool, [Char])
example7 = (5, False, "Haskell")

-- 4-tuple with a float, boolean, character, and integer:
example8 :: (Float, Bool, Char, Int)
example8 = (9.81, True, 'G', 42)

-- Tuple containing another tuple:
example9 :: ((Int, Int), String)
example9 = ((3, 4), "Coordinates")

-- 5-tuple with a mix of different types, including lists and Maybe:
example10 :: (Int, [Char], Maybe Bool, Double, [Int])
example10 = (7, "World", Just True, 2.718, [1, 2, 3])

-- Tuple with another nested tuple and a list:
example11 :: (Int, (String, [Bool]), [Float])
example11 = (42, ("Nested", [True, False, True]), [3.14, 2.71])

-- 6-tuple with different types including tuples inside a list:
example12 :: (Bool, [Int], (Char, Int), Double, String, [(Int, String)])
example12 = (True, [1, 2, 3], ('A', 65), 3.1415, "Data", [(1, "One"), (2, "Two")])