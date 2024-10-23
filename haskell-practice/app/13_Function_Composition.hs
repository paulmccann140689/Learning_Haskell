-- Function composition allows us to combine multiple functions into a single function. 
-- In Haskell, the function composition operator (.) is used to create new functions by chaining existing functions together, 
-- where the output of one function serves as the input to the next. 

-- function composition with the operator (.) follows the right-to-left evaluation order

-- The composition operator (.) has the following type signature:
-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- It takes two functions, f and g.
-- f has a type b -> c (meaning it takes an input of type b and returns an output of type c).
-- g has a type a -> b (meaning it takes an input of type a and returns an output of type b).
-- The result is a new function that has the type a -> c.

-- Can alo be written as:
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- f (.) g = \x -> f(g(x))


addOneFunc :: Int -> Int
addOneFunc x = x + 1

doubleFunc :: Int -> Int
doubleFunc x = x * 2

functionCompositionExample :: Int
functionCompositionExample = (addOneFunc . doubleFunc) 3

-- doubleFunc 3 is evaluated first, yielding 6.
-- Then, addOneFunc is applied to 6, resulting in 7.
-- The expression (addOneFunc . doubleFunc) 3 is equivalent to addOneFunc (doubleFunc 3).
-- The composed function (addOneFunc . doubleFunc) takes an integer, doubles it, and then adds 1 to the result.



-- Squaring and Adding Five
squareFunc :: Int -> Int
squareFunc x = x * x

addFiveFunc :: Int -> Int
addFiveFunc x = x + 5

compositionExample1 :: Int
compositionExample1 = (addFiveFunc . squareFunc) 4 -- Result: 21




-- Negating and Doubling
negateFunc :: Int -> Int
negateFunc x = -x

doubleFunc2 :: Int -> Int
doubleFunc2 x = x * 2

compositionExample2 :: Int
compositionExample2 = (negateFunc . doubleFunc2) 3 -- Result: -6




-- Adding Two and Tripling
addTwoFunc :: Int -> Int
addTwoFunc x = x + 2

tripleFunc :: Int -> Int
tripleFunc x = x * 3

compositionExample3 :: Int
compositionExample3 = (tripleFunc . addTwoFunc) 4 -- Result: 18




-- Calculating Square Root of a Doubled Number
doubleFunc3 :: Double -> Double
doubleFunc3 x = x * 2

sqrtFunc :: Double -> Double
sqrtFunc x = sqrt x

compositionExample4 :: Double
compositionExample4 = (sqrtFunc . doubleFunc3) 16 -- Result: 5.656854




-- Converting to String and Calculating Length
toStringFunc :: Int -> String
toStringFunc x = show x

lengthFunc :: String -> Int
lengthFunc x = length x

compositionExample5 :: Int
compositionExample5 = (lengthFunc . toStringFunc) 12345 -- Result: 5




-- Checking Even and Converting Result to String
isEvenFunc :: Int -> Bool
isEvenFunc x = x `mod` 2 == 0

boolToStringFunc :: Bool -> String
boolToStringFunc b = if b then "True" else "False"

compositionExample6 :: String
compositionExample6 = (boolToStringFunc . isEvenFunc) 4 -- Result: "True"




-- Adding Five, Squaring, and Converting to String
addFiveFunc2 :: Int -> Int
addFiveFunc2 x = x + 5

squareFunc2 :: Int -> Int
squareFunc2 x = x * x

toStringFunc2 :: Int -> String
toStringFunc2 x = show x

compositionExample7 :: String
compositionExample7 = (toStringFunc2 . squareFunc2 . addFiveFunc2) 3 -- Result: "64"




-- Taking Absolute Value, Doubling, and Converting to Boolean (If > 10)
absFunc :: Int -> Int
absFunc x = abs x

doubleFunc4 :: Int -> Int
doubleFunc4 x = x * 2

greaterThanTenFunc :: Int -> Bool
greaterThanTenFunc x = x > 10

compositionExample8 :: Bool
compositionExample8 = (greaterThanTenFunc . doubleFunc4 . absFunc) (-6) -- Result: True




-- Tripling, Calculating Modulo 7, and Checking if Even
tripleFunc2 :: Int -> Int
tripleFunc2 x = x * 3

modSevenFunc :: Int -> Int
modSevenFunc x = x `mod` 7

isEvenFunc2 :: Int -> Bool
isEvenFunc2 x = x `mod` 2 == 0

compositionExample9 :: Bool
compositionExample9 = (isEvenFunc2 . modSevenFunc . tripleFunc2) 5 -- Result: False