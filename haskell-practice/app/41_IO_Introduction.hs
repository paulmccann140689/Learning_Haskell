
import System.IO
import Control.Monad (forM_)
import Debug.Trace (trace)
import Data.IORef


-- Functional Purity
-- =================

-- Purity refers to functions that are:
-- - **Stateless:** Always return the same result for the same input.
-- - **Total:** Defined for all inputs.
-- - **Side-effect free:** Do not interact with the outside world (no I/O).


-- Challenges with Purity
-- ======================
-- To perform I/O operations (like reading input, printing output, and file operations), we encounter side effects that violate purity.  
-- Directly mixing pure and impure code can lead to:
-- - Loss of Church-Rosser property (non-determinism).
-- - Loss of referential transparency (ability to substitute code with its value).
-- - Issues with lazy evaluation and reasoning about code.


-- Main Function
-- =============
-- The entry point of every Haskell program is `main`, which has the type `IO ()`.  
-- It orchestrates the execution of IO computations.


-- Escaping IO
-- ===========
-- You cannot directly extract pure values from IO computations.  
-- The concept of "escaping" IO implies misusing functions that treat IO computations as pure, leading to potential issues.


-- IO Types
-- ========
-- IO is used to denote computations that perform side effects.  
-- Types in Haskell:
-- - `putStrLn :: String -> IO ()`: A function that prints a string and returns an IO action with no meaningful value.
-- - `getLine :: IO String`: An IO action that produces a string.

-- A String is not the same as an IO String!
-- A String is data.
-- An IO String is a computation which produces a String.


-- Easy example: Basic Console I/O
main :: IO ()
main = do
    putStrLn "Please enter your name: "  -- Prompt the user for input
    name <- getLine                     -- Read the input from the user
    putStrLn ("Hello, " ++ name ++ "!") -- Output a greeting
-- putStrLn is used to print a message to the console.
-- getLine reads a line of input from the user and binds it to the variable name.
-- We then concatenate the string "Hello, " with the user’s name and print it.


-- Medium example: File I/O
main2 :: IO ()
main2 = do
    -- Open a file for reading
    handle <- openFile "example.txt" ReadMode
    contents <- hGetContents handle      -- Read the file contents
    putStrLn contents                    -- Print the contents to the console
    hClose handle                        -- Close the file handle
-- We import System.IO at the top of the file for file handling functions.
-- openFile opens a file named example.txt in read mode.
-- hGetContents reads the contents of the file and binds it to contents.
-- We print the contents to the console using putStrLn.
-- Finally, we close the file handle using hClose to free up system resources.



-- Hard example: Combining I/O with Functional Logic
-- Function to calculate the sum of numbers from a list of strings
sumNumbers :: [String] -> Int
sumNumbers = sum . map read
main3 :: IO ()
main3 = do
    -- Open input and output files
    inputHandle <- openFile "numbers.txt" ReadMode
    outputHandle <- openFile "result.txt" WriteMode

    contents <- hGetContents inputHandle -- Read the file contents
    let numbers = lines contents          -- Split the contents into lines
    let total = sumNumbers numbers        -- Calculate the sum

    hPutStrLn outputHandle ("Total: " ++ show total) -- Write the result to the output file

    -- Close the file handles
    hClose inputHandle
    hClose outputHandle
    putStrLn "The total has been written to result.txt."
-- We define a function sumNumbers that takes a list of strings, converts them to integers using map read, and sums them up.
-- We open an input file named numbers.txt for reading and an output file named result.txt for writing.
-- The contents of the input file are read and split into lines, which are processed to calculate their sum.
-- We write the total to the output file using hPutStrLn.
-- Finally, we close both file handles and print a message indicating that the result has been written.




-- Mixing Pure Functions and IO
-- =============================
-- In a do block, use `<-` to bind results from IO actions to pure values:

-- getAndPrintReverse :: IO ()
-- getAndPrintReverse = do
--     str <- getLine
--     let revStr = reverse str
--     putStrLn revStr

-- Easy example: Reverse a String
getAndPrintReverse :: IO ()
getAndPrintReverse = do
    putStrLn "Please enter a string: "  -- Prompt the user for input
    str <- getLine                       -- Read the input
    let revStr = reverse str             -- Use a pure function to reverse the string
    putStrLn revStr                      -- Print the reversed string
main4 :: IO ()
main4 = getAndPrintReverse
-- getLine is an I/O action that reads user input.
-- reverse is a pure function that operates on the string without causing side effects.
-- The result of reverse is printed using putStrLn.



-- Medium example: Count Vowels in a String
countVowels :: String -> Int
countVowels str = length [c | c <- str, c `elem` "aeiouAEIOU"]  -- Pure function to count vowels

getAndPrintVowelCount :: IO ()
getAndPrintVowelCount = do
    putStrLn "Please enter a string: "  -- Prompt the user for input
    str <- getLine                       -- Read the input
    let vowelCount = countVowels str     -- Use a pure function to count vowels
    putStrLn ("Number of vowels: " ++ show vowelCount)  -- Print the count
main5 :: IO ()
main5 = getAndPrintVowelCount
-- countVowels is a pure function that uses list comprehension to count the vowels in a string.
-- The I/O actions (putStrLn, getLine) handle user interaction, while the counting logic remains pure.
-- The vowel count is printed to the console.



-- Hard example: Read, Process, and Write to a File
-- Pure function to calculate the sum of a list of integers
sumNumbers2 :: [Int] -> Int
sumNumbers2 = sum
-- Main I/O function
processNumbers :: IO ()
processNumbers = do
    inputHandle <- openFile "numbers.txt" ReadMode  -- Open the input file
    outputHandle <- openFile "result.txt" WriteMode  -- Open the output file

    contents <- hGetContents inputHandle               -- Read the contents of the input file
    let numbers = map read (lines contents) :: [Int]  -- Convert the lines to integers
    let total = sumNumbers2 numbers                     -- Use a pure function to calculate the sum

    hPutStrLn outputHandle ("Total: " ++ show total) -- Write the result to the output file

    hClose inputHandle                                  -- Close the input file
    hClose outputHandle                                  -- Close the output file
    putStrLn "The total has been written to result.txt."  -- Confirm completion
main6 :: IO ()
main6 = processNumbers
-- sumNumbers is a pure function that takes a list of integers and returns their sum.
-- The processNumbers function handles file I/O:
-- It opens an input file (numbers.txt) and an output file (result.txt).
-- It reads the contents of the input file, converts each line to an integer, and calculates the sum using the pure function.
-- Finally, it writes the total to the output file and closes the files.





-- Debugging and Beyond Console IO
-- =================================
-- Use `trace` for debugging (not recommended for regular use):

-- trace :: String -> a -> a
-- IO encompasses various operations, such as file handling and random number generation.


-- Function that calculates the square of a number with debugging
square :: Int -> Int
square x = trace ("Calculating square of " ++ show x) (x * x)
main7 :: IO ()
main7 = do
    let num = 5
    let result = square num
    putStrLn ("The square of " ++ show num ++ " is " ++ show result)
-- The trace function outputs a debug message before returning the result of the square calculation.
-- When you run this program, it will print the debugging message to the console, showing the value being calculated.
-- This helps you see what’s happening internally without changing the function's output.



-- Function to double a number with debugging
double :: Int -> Int
double x = trace ("Doubling " ++ show x) (x * 2)

-- Main I/O function
processFile :: IO ()
processFile = do
    handle <- openFile "numbers.txt" ReadMode  -- Open the input file
    contents <- hGetContents handle               -- Read the file contents
    let numbers = map read (lines contents) :: [Int]  -- Convert lines to integers
    let doubledNumbers = map double numbers          -- Double each number with debugging

    -- Print original and doubled numbers
    putStrLn "Original and doubled numbers:"
    mapM_ (\(orig, dbl) -> putStrLn (show orig ++ " -> " ++ show dbl)) (zip numbers doubledNumbers)

    hClose handle                                   -- Close the input file
main8 :: IO ()
main8 = processFile
-- The double function uses trace to print the value being doubled.
-- The program reads integers from a file (numbers.txt), doubles each value, and prints both the original and doubled numbers.
-- This allows you to see the intermediate values during the computation.





-- Random Number Generation
-- =========================
-- Pseudo-random number generators (PRNG) are pure once seeded. Only the seeding is impure.

-- import System.Random
-- -- Generate a single random number between 1 and 100
-- generateSingleRandom :: IO Int
-- generateSingleRandom = do
--     gen <- newStdGen                      -- Create a new random number generator
--     let (randomNumber, _) = randomR (1, 100) gen  -- Generate a random number in the range
--     return randomNumber                    -- Return the generated number
-- main :: IO ()
-- main = do
--     randomNumber <- generateSingleRandom   -- Call the function to generate a random number
--     putStrLn ("Random number: " ++ show randomNumber)  -- Print the random number








-- Reference Cells
-- =================
-- Haskell can use mutable reference cells with:
-- - `newIORef`, `readIORef`, and `writeIORef` for controlled mutability.


-- Main function to demonstrate IORefs
main10 :: IO ()
main10 = do
    -- Create a new IORef initialized to 0
    counter <- newIORef 0

    -- Read the initial value of the counter
    initialValue <- readIORef counter
    putStrLn ("Initial counter value: " ++ show initialValue)

    -- Update the counter value
    writeIORef counter 10

    -- Read the updated value of the counter
    updatedValue <- readIORef counter
    putStrLn ("Updated counter value: " ++ show updatedValue)

    -- Increment the counter value by 1
    modifyIORef counter (+1)

    -- Read the final value of the counter
    finalValue <- readIORef counter
    putStrLn ("Final counter value: " ++ show finalValue)
-- newIORef 0 creates a new mutable reference cell initialized to 0.
-- readIORef counter retrieves the current value of the counter and binds it to initialValue.
-- writeIORef counter 10 updates the value of the counter to 10.
-- We then read the updated value and print it.
-- modifyIORef counter (+1) increments the current value of counter by 1.
-- Finally, we read and print the final value of the counter.


-- Clarifications
-- =============================
-- Difference between putStrLn, show, and print

-- putStrLn: This function is used to output a string to the console. 
-- It performs an I/O action and adds a newline character after printing. 
-- Because it's an I/O action, its type is IO (), 
-- indicating it doesn't return a value that you can use further in your program.

-- show: This is a pure function that takes a value of any type that is an instance of the Show type class 
-- and converts it to its string representation. Since it’s a pure function, 
-- it doesn’t have side effects, and the output only depends on its input. 
-- For example, show 42 results in the string "42".

-- print: This function combines the functionalities of show and putStrLn. 
-- It takes a value, converts it to a string using show, and then prints that string to the console, followed by a newline. 
-- The type of print is also IO (), like putStrLn, because it performs an I/O action without returning a useful result.


-- Understanding IO () vs. IO a

-- IO (): This type indicates an I/O action that does not produce a meaningful result. 
-- The () (unit type) is used in Haskell to signify "no value." An example is when you use putStrLn, 
-- which performs an action (printing to the console) but doesn't return anything that you would use later in your program.

-- IO a: In contrast, IO a represents an I/O action that results in a value of type a. 
-- This means the action will yield a meaningful result after execution. For example, if you read a line from the console using getLine, 
--     it returns an IO String, indicating that you’ll get a string value when the action is completed.


-- Function Types and Classes

-- Type Classes: In Haskell, type classes are a way to define a set of functions that can operate on different types. 
-- They enable ad-hoc polymorphism, allowing you to write generic code that can work with any data type that 
-- implements the functions of that type class. For example, the Eq type class allows types to support equality checks, 
-- while the Show type class allows types to be converted to strings. This concept is somewhat similar to 
-- interfaces in other programming languages, where you define a contract that implementing types must fulfill.
