-- In Haskell, functions are first-class citizens, meaning they can be passed as arguments, 
-- returned from other functions, and stored in data structures. 
-- Functions are defined with a specific type signature that describes their inputs and outputs.

-- Structure of a Function
-- The general structure of a function in Haskell is as follows:

-- Type Signature: This defines the types of the input parameters and the return type of the function.
-- Example: add :: Int -> Int -> Int indicates that add takes two Int values as input and returns an Int.

-- Function Name: The name of the function.
-- Example: add.

-- Parameters: These are the inputs the function takes.
-- Example: x and y in the add function.

-- Equals Sign: The = sign is used to define the function's implementation.

-- Function Body: This is the expression that defines what the function does.
-- Example: x + y, which adds the two integers.

-- Type Signature: Describes the input and output types.
-- Function Name: The name used to call the function.
-- Parameters: Inputs taken by the function.
-- Equals Sign: Indicates the definition of the function.
-- Function Body: The computation or operation that the function performs.

-- Add Function
add :: Int -> Int -> Int
add x y = x + y

-- Multiply Function
multiply :: Int -> Int -> Int
multiply x y = x * y

-- Square Function
square :: Int -> Int
square x = x * x

-- IsEven Function
isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

-- Concatenate Strings
concatenate :: String -> String -> String
concatenate str1 str2 = str1 ++ str2

-- Calculate the Average
average :: Int -> Int -> Float
average x y = fromIntegral (x + y) / 2.0

-- Max of Two Numbers
maxOfTwo :: Int -> Int -> Int
maxOfTwo x y = if x > y then x else y

-- Factorial Function (Recursive)
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Fibonnaci Function (Recursive)
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- List Length Function
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs


main :: IO ()
main = do
    print (add 2 3)                         -- Outputs: 5
    print (multiply 4 5)                    -- Outputs: 20
    print (square 6)                        -- Outputs: 36
    print (isEven 10)                       -- Outputs: True
    print (concatenate "Hello, " "world!")  -- Outputs: "Hello, world!"
    print (average 4 8)                     -- Outputs: 6.0
    print (maxOfTwo 3 5)                    -- Outputs: 5
    print (factorial 5)                     -- Outputs: 120
    print (fibonacci 6)                     -- Outputs: 8
    print (listLength [1, 2, 3, 4])         -- Outputs: 4