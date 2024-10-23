import Test.QuickCheck
import Data.List
-- Imagine a Haskell function
-- takeThose :: (a->Bool) -> [a] -> [a]
-- which selects items from a list, if they match the given predicate.
-- So for instance,
-- takeThose (\x -> x `mod` 5 == 0) [1..20]
-- should evaluate to 
-- [5,10,15,20]
-- Write a Haskell function definition for takeThose, using a single list comprehension.
takeThose :: (a -> Bool) -> [a] -> [a]
takeThose match xs = [x | x <- xs, match x]
result = takeThose (\x -> x `mod` 5 == 0) [1..20]
-- result will be [5, 10, 15, 20]


-- Now write an alternative function definition for takeThose, 
-- using recursion over the structure of the input list parameter.
-- takeThose :: (a->Bool) -> [a] -> [a]
takeThose2 :: (a -> Bool) -> [a] -> [a]
takeThose2 _ [] = []  -- Base case: if the list is empty, return an empty list
takeThose2 predicate (x:xs)
  | predicate x = x : takeThose2 predicate xs  -- If the predicate is true for x, include it
  | otherwise   = takeThose2 predicate xs      -- Otherwise, skip x
result2 = takeThose2 (\x -> x `mod` 5 == 0) [1..20]
-- result will be [5, 10, 15, 20]


-- Now write an alternative function definition for takeThose, 
-- using a fold function over the input list parameter.
-- takeThose :: (a->Bool) -> [a] -> [a]
takeThose3 :: (a -> Bool) -> [a] -> [a]
takeThose3 predicate = foldr select []
  where
    select x acc
      | predicate x = x : acc  -- If x satisfies the predicate, include it in the accumulator
      | otherwise   = acc      -- Otherwise, just return the accumulator unchanged
result3 = takeThose3 (\x -> x `mod` 5 == 0) [1..20]
-- result will be [5, 10, 15, 20]



-- Write a Haskell function definition for the function ntimes :: [Char] -> Int -> [Char]
-- which works like Python string multiplication. So for instance, expected behaviour is:
-- ntimes "ho" 3  evaluates to  "hohoho"
-- ntimes "pass!" 2 evaluates to "pass!pass!"
-- ntimes "xxx" 0 evaluates to ""
-- ntimes "foo" (-1) evaluates to ""
-- ntimes "" 5 evaluates to ""
-- ntimes "hello" 1 evaluates to "hello"
-- Marks will be awarded for correctness, also for simplicity and elegance in Haskell.
ntimes :: [Char] -> Int -> [Char]
ntimes _ n | n <= 0    = ""            -- Return empty string for non-positive n
ntimes str n = concat (replicate n str)  -- Concatenate the string n times
main :: IO ()
main = do
    print (ntimes "ho" 3)        -- "hohoho"
    print (ntimes "pass!" 2)     -- "pass!pass!"
    print (ntimes "xxx" 0)       -- ""
    print (ntimes "foo" (-1))    -- ""
    print (ntimes "" 5)          -- ""
    print (ntimes "hello" 1)     -- "hello"


-- Recall the function ntimes :: [Char] -> Int -> [Char] from the previous question. 
-- Write a sensible quickCheck property to test the behaviour of the ntimes function.
-- Definition of the ntimes function
ntimes2 :: [Char] -> Int -> [Char]
ntimes2 _ n | n <= 0    = ""                     -- Return empty string for non-positive n
ntimes2 str n            = concat (replicate n str)  -- Concatenate the string n times

-- Property to check that ntimes behaves as expected for non-negative n
prop_ntimes :: [Char] -> Int -> Property
prop_ntimes str n = 
    n >= 0 ==> ntimes2 str n === concat (replicate n str)

-- Property to check that ntimes returns an empty string for n = 0
prop_ntimes_zero :: [Char] -> Property
prop_ntimes_zero str = ntimes2 str 0 === ""  -- Should return an empty string for n = 0

-- Property to check that ntimes returns an empty string for negative n
prop_ntimes_negative :: [Char] -> Property
prop_ntimes_negative str = ntimes2 str (-1) === ""  -- Should return an empty string for negative n

-- Property to check that ntimes returns the original string for n = 1
prop_ntimes_one :: [Char] -> Property
prop_ntimes_one str = ntimes2 str 1 === str  -- Should return the string itself for n = 1

-- Main function to run QuickCheck properties
main2 :: IO ()
main2 = do
    quickCheck prop_ntimes
    quickCheck prop_ntimes_zero
    quickCheck prop_ntimes_negative
    quickCheck prop_ntimes_one


-- Consider the following Haskell source code that models dinosaurs, 
-- which may have wings and/or spikey bodies and some integer number of legs.

-- data Dinosaur = Dinosaur Bool Bool Int deriving Eq
-- --                   ^^^ winged spikey legs

-- pterodactyl = Dinosaur True True 2
-- --    ^^  winged creature, spikey body, 2 legs

-- stegosaurus = Dinosaur False True 4
-- --  ^^  no wings, spikey body, 4 legs

-- diplodocus = Dinosaur False False 4
-- -- ^^  no wings, no spikes, 4 legs

-- Critique whether or not this is an appropriate way to model dinosaurs in Haskell. Briefly
-- present both sides of the argument and come to a conclusion.

-- Pros:
-- Simplicity: The Dinosaur type is straightforward, using three fields (two Bool values and an Int), 
-- making it easy to understand.
-- Compact Representation: Efficiently represents characteristics without needing complex structures.
-- Ease of Instantiation: Creating dinosaur instances is concise and simple.
-- Pattern Matching: Facilitates straightforward pattern matching for analysis and manipulation.

-- Cons:
-- Limited Extensibility: Adding new attributes (like color or diet) would complicate the model 
-- and could break existing code.
-- Ambiguity: Using Bool for wings and spikes may not clearly represent dinosaur diversity.
-- Readability: The model is not very self-documenting, which could confuse future developers.
-- Potential Misuse: It allows for invalid states, such as negative leg counts.




-- Now write a Show typeclass instantiation for
-- the Dinosaur datatype. You can use library functions from
-- the Prelude and the Data.List libraries if you like. Below are the 
-- expected results for calling show with
-- the predefined Dinosaur values.

-- You will see that each leg is represented with a b character.

-- Spikey dinosaurs have ^ characters between legs
-- whereas non-spikey dinosaurs have - characters.

-- Winged dinosaurs have an initial > character and a final
-- < character in their string representation .

-- ghci> map show [pterodactyl, stegosaurus, diplodocus]
-- [">b^b<","b^b^b^b","b-b-b-b"]


-- Define the Dinosaur data type
data Dinosaur = Dinosaur Bool Bool Int deriving Eq

-- Predefined Dinosaur values
pterodactyl = Dinosaur True True 2        -- Winged, spiky, 2 legs
stegosaurus = Dinosaur False True 4       -- No wings, spiky, 4 legs
diplodocus = Dinosaur False False 4       -- No wings, no spikes, 4 legs

-- Show instance for the Dinosaur type
instance Show Dinosaur where
    show (Dinosaur hasWings hasSpikes legs) = 
        let legRepresentation = replicate legs 'b'  -- Represent each leg with 'b'
            separator = if hasSpikes then "^" else "-"  -- Separator based on spikes
            formattedLegs = intercalate separator (splitIntoLegs legRepresentation legs)
        in if hasWings 
           then '>' : formattedLegs ++ "<"  -- Wrap with '>' and '<' for winged dinosaurs
           else formattedLegs  -- No wrapping for non-winged dinosaurs

-- Helper function to split leg representation into groups
splitIntoLegs :: String -> Int -> [String]
splitIntoLegs str n = 
    let legChunks = takeWhile (not . null) (map (take 1) (tails str))
    in if n > 0 then legChunks else []

-- Example usage in GHCi
main3:: IO ()
main3 = do
    print $ map show [pterodactyl, stegosaurus, diplodocus]
