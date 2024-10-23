-- Eager evaluation, also known as strict evaluation or call-by-value, evaluates expressions as soon as they are bound to variables. 
-- In this approach, all arguments to a function are evaluated before the function is executed. 
-- Most imperative and functional languages use eager evaluation.

-- Benefits:
-- Simplicity: Since everything is evaluated right away, it’s easier to predict the program's behavior.
-- Easier to combine with exception handling since the order of evaluation is known.

-- Drawbacks:
-- May lead to unnecessary computation if the results are never used.
-- Cannot handle infinite data structures.


-- Both arguments are evaluated immediately
sumStrict :: Int -> Int -> Int
sumStrict a b = a + b



-- Expensive computation evaluated immediately
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

useFirstStrict :: (a -> c) -> a -> b -> c
useFirstStrict f a b = f a  -- Both 'a' and 'b' are evaluated, even though 'b' isn't used.



-- The entire computation of an infinite list fails due to eager evaluation
-- This will result in a runtime error because eager evaluation tries to compute an infinite list
infiniteList :: [Int]
infiniteList = [1..] -- Infinite list of integers
sumList :: Int
sumList = sum infiniteList -- Tries to sum an infinite list, resulting in non-termination.
