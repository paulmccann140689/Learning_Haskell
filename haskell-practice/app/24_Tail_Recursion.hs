-- Tail recursion is a specific kind of recursion where the recursive call is the final action in the function. 
-- This technique can be highly efficient because tail-recursive functions don’t need to keep track of 
-- the intermediate states of each recursive call. Instead, they accumulate results through parameters, 
-- allowing Haskell’s runtime to optimize and prevent stack overflow even for large inputs.

-- In tail recursion, an accumulator parameter is commonly used to hold the partial result that’s progressively updated 
-- with each recursive call. The function only returns this accumulated result once all elements or steps have been processed. 
-- Haskell optimizes tail recursion through a process called tail call optimization (TCO), 
-- which reuses the stack frame of each recursive call, preventing stack growth.

-- listSumTail, demonstrates tail recursion by accumulating the sum of a list’s elements in a helper function:

listSumTail :: [Int] -> Int
listSumTail xs = helper xs 0
    where
        helper [] acc = acc
        helper (x:xs) acc = helper xs (acc + x)

-- Purpose: This function calculates the sum of all elements in a list.
-- Accumulator: acc, initialized to 0, accumulates the total as the function processes each list element.
-- Base Case: helper [] acc = acc — When the list is empty, return the accumulated sum in acc.
-- Recursive Case: helper (x:xs) acc = helper xs (acc + x) — Adds x to acc and continues with the tail of the list xs.
-- This recursive structure is efficient because acc carries the result, eliminating the need to hold intermediate computations. 
-- Once all elements are processed, acc contains the final sum.


-- Calculating Factorial Using Tail Recursion
factorialTail :: Int -> Int
factorialTail n = helper n 1
  where
    helper 0 acc = acc
    helper n acc = helper (n - 1) (n * acc)
-- Purpose: Computes the factorial of n.
-- Base Case: helper 0 acc = acc — When n reaches 0, return acc.
-- Recursive Case: helper (n - 1) (n * acc) — Multiplies n with acc and decrements n.


-- Counting Elements in a List
lengthTail :: [a] -> Int
lengthTail xs = helper xs 0
  where
    helper [] acc = acc
    helper (_:xs) acc = helper xs (acc + 1)
-- Purpose: Counts the number of elements in a list.
-- Base Case: helper [] acc = acc — When the list is empty, return the accumulated count.
-- Recursive Case: helper (_:xs) (acc + 1) — Increments acc by 1 and continues with the list’s tail.


-- Reversing a List
reverseTail :: [a] -> [a]
reverseTail xs = helper xs []
  where
    helper [] acc = acc
    helper (x:xs) acc = helper xs (x : acc)
-- Purpose: Reverses a list.
-- Base Case: helper [] acc = acc — When the list is empty, return acc as the reversed list.
-- Recursive Case: helper (x:xs) (x : acc) — Prepends x to acc and continues with the tail.


-- Computing the Greatest Common Divisor (GCD)
gcdTail :: Int -> Int -> Int
gcdTail a b = helper (abs a) (abs b)
  where
    helper a 0 = a
    helper a b = helper b (a `mod` b)
-- Purpose: Computes the GCD of two integers.
-- Base Case: helper a 0 = a — When b is 0, a is the GCD.
-- Recursive Case: helper b (a mod b) — Replaces a with b and b with a mod b.


-- Finding the Minimum Value in a List
minTail :: (Ord a) => [a] -> a
minTail [] = error "Empty list has no minimum"
minTail (x:xs) = helper xs x
  where
    helper [] acc = acc
    helper (y:ys) acc = helper ys (min y acc)
-- Purpose: Finds the smallest element in a list.
-- Base Case: helper [] acc = acc — When the list is empty, return acc as the minimum.
-- Recursive Case: helper (y:ys) (min y acc) — Compares y with acc and updates acc with the smaller value.


-- Finding the nth Fibonacci Number (Tail Recursive)
fibTail :: Int -> Int
fibTail n = helper n 0 1
  where
    helper 0 a _ = a
    helper n a b = helper (n - 1) b (a + b)
-- Purpose: Calculates the nth Fibonacci number.
-- Base Case: helper 0 a _ = a — When n reaches 0, return a.
-- Recursive Case: helper (n - 1) b (a + b) — Shifts the values of a and b in each call.


-- Flattening a Nested List Structure
data NestedList a = Elem a | List [NestedList a]

flattenTail :: NestedList a -> [a]
flattenTail xs = helper xs []
  where
    helper (Elem x) acc = x : acc
    helper (List []) acc = acc
    helper (List (x:xs)) acc = helper x (helper (List xs) acc)
-- Purpose: Flattens a nested list structure.
-- Base Case: When reaching an Elem, adds it to acc.
-- Recursive Case: Processes each element, concatenating results in acc.


-- Tail Recursive Quicksort
quickSortTail :: (Ord a) => [a] -> [a]
quickSortTail xs = helper xs []
  where
    helper [] acc = acc
    helper (p:xs) acc =
        let (smaller, larger) = partition (< p) xs
        in helper smaller (p : helper larger acc)

    partition _ [] = ([], [])
    partition f (x:xs)
        | f x       = let (ys, zs) = partition f xs in (x:ys, zs)
        | otherwise = let (ys, zs) = partition f xs in (ys, x:zs)
-- Purpose: Sorts a list using quicksort with tail recursion.
-- Base Case: When list is empty, return acc.
-- Recursive Case: Partitions based on pivot, recursively sorts partitions, and accumulates.