-- In Haskell, an equation is used to associate a name (such as a variable or function name) with an expression. 
-- Equations are fundamental in Haskell as they allow you to define constants, functions, and more complex logic 
-- in a clean, declarative style. Essentially, an equation assigns a name to an expression, 
-- and when that name is used in code, it is replaced by the associated expression.

-- There are two key components to an equation in Haskell:

-- Left-Hand Side (LHS): The name (or pattern) being defined.
-- Right-Hand Side (RHS): The expression that calculates the value for the name on the LHS.

-- Constant Definitions:
-- An equation can define a constant value. This is similar to defining variables in other programming languages, 
-- but in Haskell, these values are immutable (they cannot be changed once defined). For example:
-- In this case, favouriteNumber is defined as 5, and this cannot change during the execution of the program.

favouriteNumber :: Int
favouriteNumber = 5


-- Function Definitions:
-- Equations are commonly used to define functions. The LHS of the equation includes the function name and parameters, 
-- while the RHS defines the logic or expression that calculates the result of the function. For example:
-- Here, minExample is a function that takes two integers x and y and returns the smaller of the two values.

minExample :: Int -> Int -> Int
minExample x y = if x < y then x else y

-- Pattern Matching in Equations:
-- Equations can involve pattern matching, where different patterns on the LHS of an equation trigger different expressions on the RHS. 
-- This allows for concise and expressive code, especially in recursive or branching logic.

-- Equations Are Not Assignments:
-- Unlike imperative languages (like C, Java, or Python), where equations represent assignments that can change the state of variables, 
-- Haskell equations are purely declarative. Once a value is associated with a name, it cannot be changed. 
-- This immutability is a key feature of functional programming and helps ensure correctness and predictability in Haskell programs.

-- Examples:

-- Constant Definition
-- Defining a constant using an equation
myAge :: Int
myAge = 25
-- Usage: myAge -- Result: 25
-- In this example, myAge is a constant defined as 25




-- Simple Addition Function:
addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y
-- Usage: addTwoNumbers 3 5 -- Result: 8
-- This is a simple function using an equation that adds two numbers.




-- Basic Maximum Function:
maxExample :: Int -> Int -> Int
maxExample x y = if x > y then x else y
-- Usage: maxExample 5 10 -- Result: 10
-- Here, maxExample returns the larger of the two input numbers.




-- Absolute Value Function:
absoluteValue :: Int -> Int
absoluteValue x = if x < 0 then -x else x
-- Usage: absoluteValue (-7) -- Result: 7
-- This function computes the absolute value of a number using an equation.




-- Factorial Function Using Recursion:
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
-- Usage: factorial 5 -- Result: 120
-- This example uses recursion to define the factorial function with equations.




-- Power Function (Exponentiation):
power :: Int -> Int -> Int
power _ 0 = 1
power base exp = base * power base (exp - 1)
-- Usage: power 2 3 -- Result: 8
-- This function computes base raised to the power exp using recursion.




-- Fibonacci Sequence Using Recursion:
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
-- Usage: fibonacci 6 -- Result: 8
-- This recursive function calculates the Fibonacci sequence, 
-- using multiple equations to define the base cases and recursive case.




-- Greatest Common Divisor (GCD) Using Euclid’s Algorithm:
gcdExample :: Int -> Int -> Int
gcdExample x 0 = x
gcdExample x y = gcdExample y (x `mod` y)
-- Usage: gcdExample 56 98 -- Result: 14
-- This function uses the Euclidean algorithm to calculate the greatest common divisor of two numbers. 
-- The function is defined using multiple equations for different cases (when y is 0 and when y is not 0).



-- Merge Sort:
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys
-- Usage: mergeSort [5, 3, 8, 6, 2] -- Result: [2, 3, 5, 6, 8]
-- This is a recursive merge sort implementation using equations. It divides a list into two halves, 
-- recursively sorts them, and then merges the sorted halves.





