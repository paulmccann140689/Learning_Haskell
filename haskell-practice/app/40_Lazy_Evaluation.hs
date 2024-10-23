-- Lazy evaluation, also known as non-strict evaluation or call-by-need, 
-- defers the evaluation of an expression until its value is actually needed. 
-- This allows the creation of infinite data structures, improves performance in some cases, 
-- and can help avoid errors when dealing with undefined or invalid values.

-- Benefits:
-- Supports working with infinite data structures (e.g., lists).
-- Avoids unnecessary computations, leading to potential performance improvements.
-- Delays handling of undefined or invalid values until they are actually needed.

-- Drawbacks:
-- Harder to predict when evaluation will happen, leading to complexity in debugging and memory management.
-- Potential for memory overhead due to storing deferred computations (thunks).
-- Difficult to combine with exception handling due to unclear evaluation order.


-- The second argument is never evaluated if not used
useFirst :: a -> b -> a
useFirst x _ = x  -- The second argument is not evaluated, even if it's an expensive or infinite computation.



-- Fibonacci numbers with lazy evaluation
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
-- Taking only the first n Fibonacci numbers, leaving the rest unevaluated
takeFibs :: Int -> [Int]
takeFibs n = take n fibs



-- Infinite prime numbers with lazy evaluation using the Sieve of Eratosthenes
primes :: [Int]
primes = sieve [2..]
  where sieve (x:xs) = x : sieve [p | p <- xs, p `mod` x /= 0]
-- Taking the first n prime numbers
takePrimes :: Int -> [Int]
takePrimes n = take n primes
