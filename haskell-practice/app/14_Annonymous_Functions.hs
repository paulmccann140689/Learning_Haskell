-- Annonymous Functions
-- In Haskell, an anonymous function (also known as a lambda function) is a function that is defined without a name. 
-- They are useful when you need a quick, one-off function, and don't want to give it a formal name. 
-- These functions are defined using the \ symbol, which represents the Greek letter lambda (λ). 
-- That’s why they’re often called lambda functions in functional programming.

-- The syntax for anonymous functions in Haskell is:
-- \parameters -> expression

-- \ introduces the lambda function (you can think of it as “λ” from lambda calculus).
-- parameters are the inputs to the function (similar to named functions).
-- -> separates the parameters from the function body (the expression).

-- Examples

-- Increment a number
increment :: Int -> Int
increment = \x -> x + 1
-- Usage: increment 5  -- Result: 6


-- Multiply by 2
multiplyByTwo :: Int -> Int
multiplyByTwo = \x -> x * 2
-- Usage: multiplyByTwo 4  -- Result: 8


-- Greet a user
anonymousGreeting :: String -> String
anonymousGreeting = \name -> "Hello, " ++ name
-- Usage: anonymousGreeting "Alice"  -- Result: "Hello, Alice"


-- Square a list of numbers
squareList :: [Int] -> [Int]
squareList = map (\x -> x * x)
-- Usage: squareList [1, 2, 3, 4]  -- Result: [1, 4, 9, 16]


-- Filter even numbers
filterEvens :: [Int] -> [Int]
filterEvens = filter (\x -> x `mod` 2 == 0)
-- Usage: filterEvens [1, 2, 3, 4, 5, 6]  -- Result: [2, 4, 6]


-- Sum of  alist (Lambda version)
sumList :: [Int] -> Int
sumList = foldl (\acc x -> acc + x) 0
-- Usage: sumList [1, 2, 3, 4]  -- Result: 10


-- Annonymous function for Fibonnacci sequence
-- Fibonacci Sequence: The Fibonacci sequence is a well-known series where each number is the sum of the two preceding ones. 
-- In this example, the anonymous function recursively computes the Fibonacci number for a given n.
fibonacci :: Int -> Int
fibonacci = \n -> if n == 0 then 0 else if n == 1 then 1 else fibonacci (n - 1) + fibonacci (n - 2)
-- Usage: fibonacci 6  -- Result: 8


-- Annonymous function for finding the maximum
-- Find Maximum: The foldl1 function reduces a list by applying a binary operation between the accumulator and the next element. 
-- In this case, we check if each element x is greater than the current accumulator acc, returning the larger one.
findMax :: (Ord a) => [a] -> a
findMax = foldl1 (\acc x -> if x > acc then x else acc)
-- Usage: findMax [1, 4, 2, 8, 5]  -- Result: 8


-- Annonymous function for filtering by condition
-- Filter by Condition: This example demonstrates a higher-order anonymous function that takes a condition cond and applies 
--it to a list xs, returning only the elements that satisfy the condition.
filterByCondition :: (a -> Bool) -> [a] -> [a]
filterByCondition = \cond -> \xs -> filter cond xs
-- Usage: filterByCondition (> 5) [1, 10, 6, 3, 9]  -- Result: [10, 6, 9]