-- Mutual recursion occurs when two or more functions call each other in a recursive manner. 
-- In Haskell, this allows you to define complex behaviors by dividing them into separate, interdependent functions. 
-- Mutual recursion is particularly useful when you have closely related processes 
-- that need to work together to achieve a common result.

-- Each function performs a part of the computation and relies on the other function(s) to complete the overall task.
-- Functions can share the responsibility of handling specific cases or performing specific calculations.

-- Explanation of tick and tock
tick :: Int -> String
tick 0 = ""
tick n = "tick " ++ tock (n - 1)

tock :: Int -> String
tock 0 = ""
tock n = "tock " ++ tick (n - 1)

-- Purpose: These functions alternate between the strings "tick" and "tock" based on the integer input n.
-- Base Case: Both functions have a base case for n = 0, where they return an empty string. 
-- This prevents further recursion when the count reaches zero.
-- Recursive Case: Each function appends its respective string ("tick" or "tock") to the result of the other function, 
-- decrementing n by 1 in each call.

-- For example, calling tick 3 would give:

-- tick 3 -> "tick " ++ tock (2)
-- tock 2 -> "tock " ++ tick (1)
-- tick 1 -> "tick " ++ tock (0)
-- tock 0 -> ""
-- Resulting in: "tick tock tick tock ".


-- Counting Even and Odd Numbers
countEven :: Int -> Int
countEven 0 = 1
countEven n = countOdd (n - 1)

countOdd :: Int -> Int
countOdd 0 = 0
countOdd n = countEven (n - 1)
-- Purpose: Counts even and odd numbers based on the input n.
-- Base Cases:
-- countEven 0 returns 1 (zero is even).
-- countOdd 0 returns 0 (no odd numbers).
-- Recursive Cases: Each function calls the other to count down until reaching 0.



-- Generating a Sequence of Alternating Words
evenWord :: Int -> String
evenWord 0 = "even "
evenWord n = oddWord (n - 1)

oddWord :: Int -> String
oddWord 0 = "odd "
oddWord n = evenWord (n - 1)
-- Purpose: Produces a string of "even" and "odd" words based on n.
-- Base Cases:
-- evenWord 0 returns "even ".
-- oddWord 0 returns "odd ".
-- Recursive Cases: Calls the other function to alternate between "even" and "odd".



-- Binary Representation (0 and 1)
binary0 :: Int -> String
binary0 0 = ""
binary0 n = "0" ++ binary1 (n - 1)

binary1 :: Int -> String
binary1 0 = ""
binary1 n = "1" ++ binary0 (n - 1)
-- Purpose: Creates a string representing a binary number by alternating between "0" and "1".
-- Base Cases:
-- binary0 0 and binary1 0 both return an empty string.
-- Recursive Cases: Each function adds "0" or "1" and calls the other function with decremented n.



-- Generating a Pattern of Symbols
patternX :: Int -> String
patternX 0 = ""
patternX n = "X " ++ patternY (n - 1)

patternY :: Int -> String
patternY 0 = ""
patternY n = "Y " ++ patternX (n - 1)
-- Purpose: Creates a string of alternating "X" and "Y" symbols.
-- Base Cases: Both functions return an empty string when n = 0.
-- Recursive Cases: Each function calls the other while decrementing n.



-- Calculating Fibonacci Numbers:  Standard implementation
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Mutual recursion:
fibEven :: Int -> Int
fibEven 0 = 0
fibEven n = fibOdd (n - 1)

fibOdd :: Int -> Int
fibOdd 0 = 1
fibOdd n = fibEven (n - 1) + fibEven (n - 2)
-- Purpose: Computes Fibonacci numbers by separating even and odd indices.
-- Base Cases:
-- fibEven 0 returns 0.
-- fibOdd 0 returns 1.
-- Recursive Cases: Calls the opposite function to compute Fibonacci numbers.



-- Checking for Palindrome
isPalindrome :: String -> Bool
isPalindrome str = isEven (length str) str

isEven :: Int -> String -> Bool
isEven 0 _ = True
isEven _ "" = True
isEven n str = (head str == last str) && isOdd (n - 2) (tail (init str))

isOdd :: Int -> String -> Bool
isOdd 0 _ = True
isOdd _ "" = True
isOdd n str = (head str == last str) && isEven (n - 2) (tail (init str))
-- Purpose: Checks if a string is a palindrome using even and odd lengths.
-- Base Cases: Return True for zero or empty strings.
-- Recursive Cases: Each function compares characters and reduces the string.