-- Ad-hoc polymorphism refers to defining functions that operate differently depending on the type of their arguments. 
-- This is achieved using typeclasses. Typeclasses allow you to define a set of functions 
-- that must be implemented by any type that wants to be part of that typeclass.

-- Benefits:
-- Enables different behavior for different types while keeping the same function name.
-- Provides a flexible mechanism for overloading functions and operators for specific types.


-- Define your custom 'Display' typeclass (instead of 'Show')
class Display a where
  display :: a -> String

-- Define a custom 'Insect' type with specific values
data Insect = Spider | Ant | Centipede

-- Implement the custom 'Display' typeclass for 'Insect'
instance Display Insect where
  display Spider = "Spider"
  display Ant = "Ant"
  display Centipede = "Centipede"

-- Implement the 'Leggy' typeclass for 'Insect'
class Leggy a where
  numLegs :: a -> Int

instance Leggy Insect where
  numLegs Spider = 8
  numLegs Ant = 6
  numLegs Centipede = 100

-- Function to describe an insect
describe :: (Display a, Leggy a) => a -> String
describe x = display x ++ " has " ++ show (numLegs x) ++ " legs."

-- Example usage
main = putStrLn (describe Spider)  -- Output: "Spider has 8 legs."




-- Show: Converts a value to a human-readable String.
-- Read: Parses a String to create a value of a specific type.
-- Eq: Allows comparison of values for equality, using the (==) operator.

-- Define the Animal data type
data Animal = Dog | Cat | Bird
  deriving (Show, Read, Eq)

main2 :: IO ()
main2 = do
  -- Example of Show
  let dog = Dog
  putStrLn $ "Show example: " ++ show dog  -- Output: Show example: Dog
  
  -- Example of Read
  let catString = "Cat"
  let readCat = read catString :: Animal
  putStrLn $ "Read example: " ++ show readCat  -- Output: Read example: Cat

  -- Example of Eq
  let bird = Bird
  let isEqual = dog == bird
  putStrLn $ "Eq example: " ++ show isEqual  -- Output: Eq example: False

-- Output
-- Show example: Dog
-- Read example: Cat
-- Eq example: False

