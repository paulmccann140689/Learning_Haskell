import Data.Char (toUpper)
import Control.Monad (forever)
import System.IO (appendFile)

-- IO

-- Question 1
-- Write a function, echoCaps :: IO (), which reads in a line of text, 
-- capitalises it, and then prints it to the console. 
-- (Hint: If you write import Data.Char, you can use the toUpper :: Char -> Char
-- function along with a list comprehension to capitalise a string).

-- Function to read input, capitalize it, and print the result
echoCaps :: IO ()
echoCaps = do
    input <- getLine                        -- Read a line from the console
    let capitalized = map toUpper input     -- Capitalize the input using map and toUpper
    putStrLn capitalized                    -- Print the capitalized result

-- Function to read input, capitalize it using list comprehension, and print the result
echoCaps2 :: IO ()
echoCaps2 = do
    input <- getLine                            -- Read a line from the console
    let capitalized = [toUpper c | c <- input]  -- Capitalize the input using a list comprehension
    putStrLn capitalized                        -- Print the capitalized result


-- Question 2
-- Using the following functions (noting that FilePath is a type synonym for String):
-- • readFile :: FilePath -> IO String (which reads a file with the given path)
-- • lines :: String -> [String] (which separates a string into a list of strings)
-- Write a function echoFile :: FilePath -> IO () which prints out a file to the console, line-by-line.

-- Function to read a file and print it line by line
echoFile :: FilePath -> IO ()
echoFile filePath = do
    content <- readFile filePath    -- Read the content of the file
    let fileLines = lines content   -- Split the content into lines
    mapM_ putStrLn fileLines        -- Print each line using mapM_ (which applies an IO action to each element in a list)



-- Question 3
--  Using the getLine :: IO String, read :: String -> a, and show :: a -> String functions,
-- write a function calculator :: IO () which:
-- • Gets an operation (one of +, −, ∗) from the command line; if the input does not match one of
-- these then default to +
-- • Gets two numbers using two calls to getLine
-- • Outputs the operation applied to the two integers

-- Calculator function
calculator :: IO ()
calculator = do
    -- Get the operation from the user
    putStrLn "Enter operation (+, -, *):"
    op <- getLine

    -- Get two numbers from the user
    putStrLn "Enter first number:"
    num1Str <- getLine
    putStrLn "Enter second number:"
    num2Str <- getLine

    -- Convert strings to integers
    let num1 = read num1Str :: Int
    let num2 = read num2Str :: Int

    -- Determine the operation (default to + if not valid)
    let operation = case op of
                      "+" -> (+)
                      "-" -> (-)
                      "*" -> (*)
                      _   -> (+)  -- Default to addition

    -- Perform the operation
    let result = operation num1 num2

    -- Output the result
    putStrLn ("The result is: " ++ show result)


-- Question 4
--  Using the following functions:
-- • forever :: IO a -> IO b (which performs an IO action forever; you will need to import Control.Monad)
-- • getLine :: IO String (which reads a line from the console)
-- • appendFile :: FilePath -> String -> IO () (which appends a string to a given file)
-- Write a function infiniteAppend :: IO () which:
-- (a) Gets a file path from the console
-- (b) Gets a line from the console, and appends it to the file
-- (c) Repeats step (b)

-- Import forever function at the top of the file
-- Import appendFile for file operations at the top of the file
-- Function to repeatedly append lines to a file
infiniteAppend :: IO ()
infiniteAppend = do
    -- Get the file path from the console
    putStrLn "Enter the file path:"
    filePath <- getLine
    
    -- Use forever to continually get input and append it to the file
    forever $ do
        -- Get a line from the user
        putStrLn "Enter a line to append to the file:"
        line <- getLine
        
        -- Append the line to the specified file with a newline character
        appendFile filePath (line ++ "\n")


-- Other Monads

-- Question 1
-- The flatten :: [[a]] -> [a] function can be implemented as a list comprehension
-- flatten xss = [ x | xs <- xss, x <- xs ] allows us to flatten a nested list. Write flatten
-- using the list monad, both with do-notation and with explicit monadic notation

-- Flatten function using do-notation
flattenDoNotation :: [[a]] -> [a]
flattenDoNotation xss = do
    xs <- xss   -- For each list xs in the outer list xss
    x  <- xs   -- For each element x in the inner list xs
    return x   -- Return x, which gets collected into the resulting list


--Using explicit monadic notation, we can achieve the same result 
-- by utilizing the >>= operator, which is the bind operator for the list monad:
-- Flatten function using explicit monadic notation
flattenExplicitNotation :: [[a]] -> [a]
flattenExplicitNotation xss = xss >>= \xs -> xs  -- Bind each inner list xs and concatenate them
