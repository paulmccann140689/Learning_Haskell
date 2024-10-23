-- Concise notation allows us to generate lists of values in a compact way using ranges and list comprehensions. 
-- This is particularly helpful for creating sequences without manually specifying each element, 
-- allowing us to work efficiently with sequences of numbers, characters, or custom patterns.

-- Concise notation for list generation uses ellipses (..) to define ranges.  This allows:

-- Range generation: Creating sequences by defining start and end points.
-- Step-wise generation: Specifying custom intervals to control the step size between elements.
-- Infinite lists: Generating lists without a defined end, which can be processed lazily.

-- Basic Syntax for Concise List Generation
-- Basic Range: [start..end]
-- Generates a list from start to end, inclusive. For example, [1..5] results in [1, 2, 3, 4, 5].

-- Custom Step: [start, next..end]
-- Specifies the step between elements. For example, [1, 3..9] results in [1, 3, 5, 7, 9].

-- Infinite List: [start..]
-- Creates an infinite list starting from start and incrementing by 1 each time. 
-- Example: [1..] generates an infinite list of natural numbers.

-- Types of Concise List Generation
-- Numeric Ranges: Defining lists of numbers with specified intervals.
-- Character Ranges: Defining lists of characters within ASCII ranges.
-- Custom Patterns: Specifying patterns within the concise notation to achieve custom generation.

-- Basic Range of Integers
-- Creates a list of integers from 1 to 10.
listRangeExample :: [Int]
listRangeExample = [1..10] -- Result: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]


-- Alphabet Range
-- Creates a list of lowercase alphabet letters from a to z.
charRangeExample :: [Char]
charRangeExample = ['a'..'z'] -- Result: "abcdefghijklmnopqrstuvwxyz"


-- Even Numbers
-- Generates a list of even numbers up to 10.
evenNumbers :: [Int]
evenNumbers = [2, 4..10] -- Result: [2, 4, 6, 8, 10]


-- Descending Range
-- Generates a descending list of numbers from 10 to 1.
descendingList :: [Int]
descendingList = [10, 9..1] -- Result: [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]


-- Range of Odd Numbers
-- Creates a list of odd numbers from 1 to 19.
oddNumbers :: [Int]
oddNumbers = [1, 3..19] -- Result: [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]


-- Skipping Characters
-- Generates every third letter from a to z.
skipChars :: [Char]
skipChars = ['a', 'd'..'z'] -- Result: "adgjmpsvy"


-- Infinite List with Take
-- Generates an infinite list of multiples of 5 and takes the first 10 elements.
multiplesOfFive :: [Int]
multiplesOfFive = take 10 [5, 10..] -- Result: [5, 10, 15, 20, 25, 30, 35, 40, 45, 50]


-- Fibonacci-like Sequence
-- Generates a custom pattern similar to Fibonacci, where each element is the sum of the previous two.
fibonacciLike :: [Int]
fibonacciLike = take 10 [1, 1, 2, 3, 5, 8, 13, 21, 34, 55] -- Result: First 10 terms of Fibonacci sequence


-- Squares of Odd Numbers
-- Takes the first 5 squares of odd numbers from an infinite list.
squaresOfOdds :: [Int]
squaresOfOdds = take 5 [x^2 | x <- [1, 3..]] -- Result: [1, 9, 25, 49, 81]