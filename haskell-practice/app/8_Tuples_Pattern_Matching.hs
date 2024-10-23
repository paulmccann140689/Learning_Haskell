-- Pattern Matching With Tuples

-- Pattern matching is a powerful feature in Haskell that allows you to deconstruct data structures, 
-- including tuples, by matching the structure and extracting the values directly. 
-- When pattern matching with tuples, you explicitly define the structure of the tuple and bind its elements to variables, 
-- enabling you to use those variables in subsequent expressions.

-- Why Use Pattern Matching with Tuples?
-- Readable and Intuitive: It makes the code more readable by allowing you to directly extract the values you care about.
-- Flexible: You can match tuples of any size, and it works well with other constructs like lists, Maybe, or custom data types.
-- Type-Safe: The pattern matching ensures that the code is type-safe at compile time. If the structure doesn't match the expected pattern, it results in a compiler error.
-- Better Than fst and snd for Larger Tuples: While fst and snd are limited to pairs, pattern matching works for tuples of any arity (size).

-- Example for a 2-tuple
f (x, y) = x + y

-- Example for a 3-tuple
g (a, b, c) = a * b + c


-- IMPORTANT
-- To run each example, comment out the previous function/functions and change 'main1' or 'main2' etc to just 'main'

-- Define the function
example1 :: (Int, String) -> String
example1 (x, y) = "First element: " ++ show x ++ ", Second element: " ++ y

-- Example usage of the function
main :: IO ()
main = do
  let result1 = example1 (10, "Hello")
  putStrLn result1




-- Function that extracts only the first element and ignores the second one
example2 :: (Int, String) -> Int
example2 (x, _) = x

-- Main function to run the example
main2 :: IO ()
main2 = do
  let result2 = example2 (5, "World")
  print result2




-- Function that swaps the elements of a 2-tuple
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- Main function to run the example
main3 :: IO ()
main3 = do
  let result3 = swap (1, "apple")
  print result3




-- Function that extracts all elements from a 3-tuple and formats them into a string
example4 :: (Int, String, Bool) -> String
example4 (x, y, z) = "Int: " ++ show x ++ ", String: " ++ y ++ ", Bool: " ++ show z

-- Main function to run the example
main4 :: IO ()
main4 = do
  let result4 = example4 (42, "Answer", True)
  putStrLn result4




  -- Function that extracts the second element from tuples where the first element is greater than 10
example5 :: [(Int, String)] -> [String]
example5 xs = [y | (x, y) <- xs, x > 10]

-- Main function to run the example
main5 :: IO ()
main5 = do
  let result5 = example5 [(5, "small"), (15, "large"), (20, "huge")]
  print result5




  -- Function that deconstructs a tuple containing another tuple
example6 :: ((Int, String), Bool) -> String
example6 ((x, y), z) = "Tuple contains: " ++ show x ++ " and " ++ y ++ " with Bool: " ++ show z

-- Main function to run the example
main6 :: IO ()
main6 = do
  let result6 = example6 ((1, "nested"), True)
  putStrLn result6




  -- Function that extracts and formats elements from a 4-tuple
example7 :: (Int, String, Bool, Float) -> String
example7 (a, b, c, d) = "Int: " ++ show a ++ ", String: " ++ b ++ ", Bool: " ++ show c ++ ", Float: " ++ show d

-- Main function to run the example
main7 :: IO ()
main7 = do
  let result7 = example7 (3, "four", True, 4.5)
  putStrLn result7




  -- Function that deconstructs nested tuples
example8 :: (Int, (Bool, (String, Char))) -> String
example8 (x, (y, (z, w))) = "Int: " ++ show x ++ ", Bool: " ++ show y ++ ", String: " ++ z ++ ", Char: " ++ [w]

-- Main function to run the example
main8 :: IO ()
main8 = do
  let result8 = example8 (1, (True, ("Nested", 'A')))
  putStrLn result8




  -- Recursive function that sums pairs of integers in a list of 2-tuples
example9 :: [(Int, Int)] -> Int
example9 [] = 0
example9 ((x, y):xs) = x + y + example9 xs

-- Main function to run the example
main9 :: IO ()
main9 = do
  let result9 = example9 [(1, 2), (3, 4), (5, 6)]
  print result9
