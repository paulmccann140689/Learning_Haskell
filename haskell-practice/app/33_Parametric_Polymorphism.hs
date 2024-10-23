-- Parametric polymorphism in Haskell allows functions to operate on values without specifying their exact types. 
-- Instead, we use type variables, typically represented by lowercase letters (like a, b, etc.), as placeholders for any type. 
-- This enables functions to work with any data type, making them highly reusable and flexible.

-- In contrast to ad-hoc polymorphism (where functions are defined for specific types through type classes) 
-- or subtype polymorphism (where functions accept any type that’s a subtype of a given type), 
-- parametric polymorphism imposes no constraints on the types used. 
-- This makes it a form of "true" polymorphism, as the function behaves identically regardless of the specific type.

-- For instance, a function that returns the first element of a tuple does not need to know the types of the tuple elements, 
-- because it only returns the first one. This kind of generality is possible because of parametric polymorphism.

-- In Haskell, parametric polymorphic functions often have type signatures with type variables. Here are a few examples to illustrate:

-- firstElement :: a -> b -> a
-- This function takes two arguments of any type and returns the first one.
-- swap :: (a, b) -> (b, a)
-- This function takes a tuple of any two types and swaps the elements.


firstElement :: a -> b -> a
firstElement x _ = x

reverseList :: [a] -> [a]
reverseList = reverse

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- firstElement takes two arguments of any types (a and b) and returns the first argument (a), discarding the second.
-- reverseList reverses a list of any type of elements without knowing or needing to know the type of elements in the list.
-- swap takes a tuple of two elements of any types and returns the tuple with elements in reversed order.


-- Identity Function
-- The identity function returns its argument unchanged, regardless of type.
identity :: a -> a
identity x = x

identityExample :: Int
identityExample = identity 5 -- Result: 5



-- Constant Function
-- Returns the first of two arguments, discarding the second.
constFunc :: a -> b -> a
constFunc x _ = x

constExample :: String
constExample = constFunc "Hello" True -- Result: "Hello"




-- List Length
-- A function to calculate the length of a list, regardless of the type of elements in the list.
listLength :: [a] -> Int
listLength xs = length xs

lengthExample :: Int
lengthExample = listLength [1, 2, 3, 4] -- Result: 4



-- Head of List
-- A function to get the head of a list of any type.
headOfList :: [a] -> a
headOfList (x:_) = x
headOfList [] = error "Empty list"

headExample :: Char
headExample = headOfList "Haskell" -- Result: 'H'




-- Duplicate Element in a Pair
-- Creates a tuple where both elements are the same as the given argument.
duplicate :: a -> (a, a)
duplicate x = (x, x)

duplicateExample :: (Bool, Bool)
duplicateExample = duplicate True -- Result: (True, True)




-- List Mapping
-- Applies a function to every element of a list.
mapList :: (a -> b) -> [a] -> [b]
mapList f xs = map f xs

mapExample :: [Int]
mapExample = mapList (+1) [1, 2, 3] -- Result: [2, 3, 4]




-- Apply Function Twice
-- A function that applies another function twice to an argument.
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

twiceExample :: Int
twiceExample = applyTwice (+2) 5 -- Result: 9




-- Flatten a List of Lists
-- Flattens a list of lists into a single list.
flatten :: [[a]] -> [a]
flatten xss = concat xss

flattenExample :: [Int]
flattenExample = flatten [[1, 2], [3, 4], [5]] -- Result: [1, 2, 3, 4, 5]




-- Filter with Predicate
-- Filters elements in a list based on a predicate function.
filterList :: (a -> Bool) -> [a] -> [a]
filterList p xs = filter p xs

filterExample :: [Int]
filterExample = filterList even [1, 2, 3, 4, 5] -- Result: [2, 4]