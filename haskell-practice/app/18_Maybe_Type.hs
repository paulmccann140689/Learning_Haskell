-- The Maybe type is an example of an algebraic data type used to represent computations that might fail 
-- or may not return a value. It is particularly useful for handling optional values 
-- without resorting to using null or similar constructs, which can lead to runtime errors if not handled properly.

-- The Maybe type is defined as follows:

-- data Maybe a = Nothing | Just a
-- Breakdown of the Maybe Type

-- Constructors:
-- Nothing: This constructor represents the absence of a value. 
-- It indicates that a computation has failed or that there is no value to return.
-- Just a: This constructor wraps a value of type a, indicating that a valid value exists. 
-- The type parameter a can be any type, allowing Maybe to be used flexibly with different data types.

-- Use Cases:
-- The Maybe type is commonly used in situations where a function might not be able to produce a meaningful result. 
-- For instance, looking up a value in a data structure, accessing the head of a list, 
-- or finding an element in a collection.

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- Type Signature:
-- safeHead :: [a] -> Maybe a: This indicates that the function takes a list of any type a 
-- and returns a Maybe type of the same type a.

-- Pattern Matching:
-- safeHead [] = Nothing: If the input list is empty, the function returns Nothing, 
-- indicating there is no head element to return.

-- safeHead (x:_) = Just x: If the list is non-empty, 
-- the function uses pattern matching to bind the head of the list (x) and returns Just x, 
-- wrapping the value in the Maybe type.


-- Safe Tail Function
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs
-- Explanation: Returns the tail of a list wrapped in Maybe. 
-- If the list is empty, it returns Nothing; otherwise, it returns the tail in Just.



-- Safe Lookup in a List
safeLookup :: Eq a => a -> [(a, b)] -> Maybe b
safeLookup _ [] = Nothing
safeLookup key ((k,v):xs)
    | key == k  = Just v
    | otherwise = safeLookup key xs
-- Explanation: Safely looks up a value by key in a list of key-value pairs. 
-- Returns Nothing if the key is not found; otherwise, returns the value wrapped in Just.



-- Get the First Character of a String
safeFirstChar :: String -> Maybe Char
safeFirstChar [] = Nothing
safeFirstChar (x:_) = Just x
-- Explanation: Similar to safeHead, this function retrieves the first character of a string, 
-- returning Nothing for an empty string.



-- Division with Safety
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)
-- Explanation: Performs division, returning Nothing if the divisor is zero, preventing a runtime error.



-- Find the Maximum Value in a List
safeMax :: Ord a => [a] -> Maybe a
safeMax [] = Nothing
safeMax xs = Just (maximum xs)
-- Explanation: Returns the maximum value from a list. Returns Nothing for an empty list; otherwise, 
-- it wraps the maximum value in Just.



-- Safe Character Access in a String
safeCharAt :: String -> Int -> Maybe Char
safeCharAt s i
    | i < 0 || i >= length s = Nothing
    | otherwise = Just (s !! i)
-- Explanation: Safely retrieves a character at a given index in a string. Returns Nothing for out-of-bounds indices.




-- Safe Merge of Two Lists
safeMerge :: [a] -> [a] -> Maybe [a]
safeMerge [] ys = Just ys
safeMerge xs [] = Just xs
safeMerge xs ys = Just (xs ++ ys)
-- Explanation: Merges two lists safely. Returns Nothing if both lists are empty, 
--     but otherwise wraps the merged result in Just.
