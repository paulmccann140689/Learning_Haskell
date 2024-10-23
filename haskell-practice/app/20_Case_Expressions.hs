-- A case expression evaluates an expression based on pattern matches. 
-- It allows for a series of patterns to check against the value of an expression, where each pattern has a corresponding result. 
-- This is particularly useful when dealing with data types where you want different behavior based on the structure or value.

-- The syntax follows this form:

-- case expression of
--     pattern1 -> result1
--     pattern2 -> result2
--     ...
--     _        -> defaultResult

-- expression: The expression to match on.
-- Patterns (pattern1, pattern2, etc.): These patterns represent possible values or structures of expression. 
-- If expression matches a pattern, the corresponding result is executed.
-- Results: Each pattern has a result expression that evaluates if the pattern matches.
-- Wildcard (_): Used as a catch-all pattern for unmatched cases, providing a default result


describeNum :: Int -> String
describeNum num = case num of
    1 -> "one"
    2 -> "two"
    3 -> "three"
    _ -> "something else"

-- describeNum takes an integer, num, and matches it to a specific pattern.
-- For numbers 1, 2, or 3, it returns "one", "two", or "three" respectively.
-- If num is anything else, it falls to the _ (wildcard) pattern and returns "something else".


describeList :: [a] -> String
describeList lst = case lst of
    [] -> "empty"
    [_] -> "one element"
    _ -> "more elements"

-- describeList takes a list, lst, and matches it to patterns describing the list's length.
-- If lst is an empty list ([]), it returns "empty".
-- If lst has exactly one element ([_]), it returns "one element".
-- Any other list (captured by _) returns "more elements".



-- Boolean Description
describeBool :: Bool -> String
describeBool b = case b of
    True  -> "True value"
    False -> "False value"
-- This simple case expression describes a boolean value as either "True value" or "False value".



-- Describe Day of Week
describeDay :: Int -> String
describeDay day = case day of
    1 -> "Monday"
    2 -> "Tuesday"
    3 -> "Wednesday"
    4 -> "Thursday"
    5 -> "Friday"
    6 -> "Saturday"
    7 -> "Sunday"
    _ -> "Invalid day"
-- Matches integers to day names, with a catch-all for invalid numbers.




-- Describe Sign of Number
describeSign :: Int -> String
describeSign n = case compare n 0 of
    LT -> "Negative"
    EQ -> "Zero"
    GT -> "Positive"
-- Uses compare to check if a number is negative, zero, or positive.



-- Classify Grade
classifyGrade :: Int -> String
classifyGrade grade = case grade of
    n | n >= 90 -> "A"
    n | n >= 80 -> "B"
    n | n >= 70 -> "C"
    n | n >= 60 -> "D"
    _           -> "F"
-- Uses guards with case expressions to classify numeric grades.




-- Describe Tuple
describeTuple :: (Int, Int) -> String
describeTuple t = case t of
    (0, 0) -> "Both zero"
    (0, _) -> "First is zero"
    (_, 0) -> "Second is zero"
    _      -> "Neither zero"
-- Matches tuples based on specific values, with a general case.



-- List Length Description
describeListLength :: [a] -> String
describeListLength xs = case length xs of
    0 -> "Empty list"
    1 -> "Single item list"
    2 -> "Two items"
    _ -> "Long list"
-- Describes list length based on pattern matching with length.



-- Binary Tree Depth
data Tree a = Leaf | Node a (Tree a) (Tree a)

describeTreeDepth :: Tree a -> String
describeTreeDepth tree = case tree of
    Leaf           -> "Leaf node"
    Node _ Leaf Leaf -> "One level"
    Node _ _ Leaf  -> "Two levels"
    Node _ _ _     -> "More than two levels"
-- A case expression that patterns matches on the structure of a binary tree.



-- Categorize Person by Age
data Person = Person { name :: String, age :: Int }

describePersonAge :: Person -> String
describePersonAge person = case age person of
    n | n < 13   -> name person ++ " is a child"
    n | n < 20   -> name person ++ " is a teenager"
    n | n < 65   -> name person ++ " is an adult"
    _            -> name person ++ " is a senior"
-- Uses the age field of a Person record to categorize age groups with guards.



-- Pattern Matching with Nested Lists
describeNestedList :: [[a]] -> String
describeNestedList lst = case lst of
    []         -> "No lists"
    [[]]       -> "One empty list"
    [_:_:_]    -> "Contains a list with at least two elements"
    (_:_:_:_)  -> "Contains a nested list with three or more items"
    _          -> "Other nested list structure"
-- A complex case expression that patterns matches on a nested list’s structure.