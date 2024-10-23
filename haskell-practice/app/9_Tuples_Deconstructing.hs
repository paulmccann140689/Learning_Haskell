-- Deconstructing tuples using fst and snd

-- In Haskell, fst and snd are functions that work with pairs (also called tuples of two elements). 
-- These functions allow you to extract the first and second components of a pair, respectively.

-- fst :: (a, b) -> a
-- snd :: (a, b) -> b

-- These functions only work with pairs (tuples with exactly two elements). 
-- For tuples with more than two elements, you would need to use pattern matching or other functions.

-- fst: Takes a pair and returns the first element.
fstExample1 :: Int
fstExample1 = fst (1, "hello")

-- snd: Takes a pair and returns the second element.
sndExample :: String
sndExample = snd (1, "hello")


-- Defining a pair (tuple) called 'myPair'
myPair :: (Int, String)
myPair = (10, "Hello")

-- Extracting the first element using 'fst'
firstElement :: Int
firstElement = fst myPair  -- This will give 10

-- Extracting the second element using 'snd'
secondElement :: String
secondElement = snd myPair  -- This will give "Hello"