-- Recursion allows functions to operate over complex data structures 
-- and execute repetitive tasks without explicit looping constructs like for or while loops. 
-- Haskell uses recursion extensively, often alongside pattern matching, 
-- making it powerful and expressive for processing lists and other data structures.

-- A recursive function calls itself, breaking down a problem into smaller instances of the same problem. 
-- Recursion relies heavily on base cases and recursive cases:

-- Base Case: The simplest instance of the problem, which can be solved directly and stops further recursion.
-- Recursive Case: The function reduces the problem size, calling itself on a smaller portion until it reaches the base case.

-- Example Explanation: listSum

-- listSum :: [Int] -> Int
-- listSum [] = 0
-- listSum (x:xs) = x + listSum xs

-- Function Purpose: listSum takes a list of integers and returns their total sum.
-- Base Case: listSum [] = 0 — When given an empty list, it returns 0, as there’s nothing to sum.
-- Recursive Case: listSum (x:xs) = x + listSum xs — For a non-empty list, 

-- it breaks the list into the HEAD ELEMENT: 'x' and the TAIL: 'xs'. It adds x to the result of recursively calling listSum on xs.
-- The recursion continues until it reaches an empty list ([]), at which point the function "unwinds" and computes the total sum.



-- Calculating Factorial
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
-- Purpose: Computes the factorial of a non-negative integer.
-- Base Case: factorial 0 = 1 — Factorial of 0 is defined as 1.
-- Recursive Case: factorial n = n * factorial (n - 1) — Multiplies n by the factorial of n-1.



-- Count Elements in a List
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs
-- Purpose: Counts the number of elements in a list.
-- Base Case: listLength [] = 0 — An empty list has a length of 0.
-- Recursive Case: listLength (_:xs) = 1 + listLength xs — Adds 1 for each element and continues with the tail xs.



-- Reverse a List
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]
-- Purpose: Reverses a list.
-- Base Case: reverseList [] = [] — An empty list is already reversed.
-- Recursive Case: reverseList (x:xs) = reverseList xs ++ [x] — Moves the head to the end and reverses the tail.



-- Finding the Maximum Element in a List
listMax :: [Int] -> Int
listMax [x] = x
listMax (x:xs) = max x (listMax xs)
-- Purpose: Finds the maximum value in a list of integers.
-- Base Case: listMax [x] = x — A single-element list’s maximum is the element itself.
-- Recursive Case: listMax (x:xs) = max x (listMax xs) — Compares the head x with the max of the tail xs.



-- Fibonacci Sequence
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
-- Purpose: Computes the nth Fibonacci number.
-- Base Cases: fibonacci 0 = 0 and fibonacci 1 = 1 — The first two Fibonacci numbers are 0 and 1.
-- Recursive Case: fibonacci n = fibonacci (n - 1) + fibonacci (n - 2) — Sum of the two preceding numbers.



-- Check if Element Exists in List
elemInList :: Eq a => a -> [a] -> Bool
elemInList _ [] = False
elemInList y (x:xs)
    | y == x    = True
    | otherwise = elemInList y xs
-- Purpose: Checks if an element is present in a list.
-- Base Case: elemInList _ [] = False — Element is not in an empty list.
-- Recursive Case: Compares y to the head x, returning True if they’re equal; otherwise, continues with the tail.



-- Binary Tree Depth
data Tree a = Leaf | Node a (Tree a) (Tree a)

treeDepth :: Tree a -> Int
treeDepth Leaf = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)
-- Purpose: Computes the depth of a binary tree.
-- Base Case: treeDepth Leaf = 0 — A leaf node has depth 0.
-- Recursive Case: Adds 1 to the maximum depth of the left and right subtrees.



-- Quick Sort
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let smaller = quickSort [y | y <- xs, y <= x]
        larger  = quickSort [y | y <- xs, y > x]
    in  smaller ++ [x] ++ larger
-- Purpose: Sorts a list using the quick sort algorithm.
-- Base Case: quickSort [] = [] — An empty list is already sorted.
-- Recursive Case: Uses list comprehension to partition the list into elements smaller and larger than the pivot (x), 
-- then recursively sorts each part.