-- In Haskell, where bindings provide a way to define local variables and functions at the end of a function's definition. 
-- This approach makes code cleaner by placing the main expression at the top and allowing supporting calculations to follow.

-- Unlike let bindings, which are part of an expression and scoped only within that expression,
--  where bindings are associated directly with the function they’re defined in. 
--  They are visible to all expressions within that function.


-- Syntax and Structure

-- functionName arguments = expression
--   where
--     variable1 = expression1
--     variable2 = expression2
--     -- and so on...


-- Function Header: The main expression is placed at the beginning, making it clear what the function does.

-- Where Block: Supports a series of definitions, which can be variables or functions. 
-- These definitions provide values or perform calculations that the main expression requires.


-- Example Breakdown
-- The example below, quadraticRootsWhere, uses where to calculate the roots of a quadratic equation:

-- quadraticRootsWhere :: Floating a => a -> a -> a -> [a]
-- quadraticRootsWhere a b c = [rootp, rootm]
--   where
--     det2 = b * b - 4 * a * c
--     det = sqrt det2
--     rootp = (-b + det) / (2 * a)
--     rootm = (-b - det) / (2 * a)

-- Step-by-step:

-- The main expression [rootp, rootm] describes the two roots.
-- The where block defines three intermediate values:
-- det2: The discriminant b^2 - 4ac
-- det: The square root of the discriminant
-- rootp and rootm: The positive and negative solutions for the quadratic equation, respectively.
-- These values are used in calculating the result list, [rootp, rootm].



-- Finding the Area of a Rectangle
-- Uses a where binding to separate width and height.
areaWhere :: Int -> Int -> Int
areaWhere width height = area
  where
    area = width * height -- Usage: areaWhere 4 5 -- Result: 20



-- Converting Celsius to Fahrenheit
-- Separates the conversion formula within where.
celsiusToFahrenheit :: Floating a => a -> a
celsiusToFahrenheit c = fahrenheit
  where
    fahrenheit = (c * 9 / 5) + 32 -- Usage: celsiusToFahrenheit 25 -- Result: 77.0



-- Calculating Square and Cube
-- Uses where to compute square and cube for easy access.
squareAndCube :: Int -> (Int, Int)
squareAndCube x = (square, cube)
  where
    square = x * x
    cube = x * x * x -- Usage: squareAndCube 3 -- Result: (9,27)



-- Calculate a Discounted Price
-- Calculates discount and final price with where bindings.
discountedPrice :: Floating a => a -> a -> a
discountedPrice price discountPercent = finalPrice
  where
    discount = price * (discountPercent / 100)
    finalPrice = price - discount -- Usage: discountedPrice 100 10 -- Result: 90.0



-- Pythagorean Triple Verification
-- Uses where to calculate squared terms for checking if three numbers form a Pythagorean triple.
isPythagoreanTriple :: Int -> Int -> Int -> Bool
isPythagoreanTriple a b c = aSquared + bSquared == cSquared
  where
    aSquared = a * a
    bSquared = b * b
    cSquared = c * c -- Usage: isPythagoreanTriple 3 4 5 -- Result: True



-- Sum of Squares of Odd Numbers in a List
-- Filters odd numbers and calculates their squares.
sumOddSquares :: [Int] -> Int
sumOddSquares nums = sumSquares
  where
    oddNums = filter odd nums
    squares = map (^2) oddNums
    sumSquares = sum squares -- Usage: sumOddSquares [1,2,3,4,5] -- Result: 35



-- Computing Compound Interest
-- Calculates future value with where to separate formula components.
compoundInterestWhere :: Floating a => a -> a -> Int -> a
compoundInterestWhere principal rate years = futureValue
  where
    growthFactor = 1 + rate
    futureValue = principal * (growthFactor ^ years) -- Usage: compoundInterestWhere 1000 0.05 10 -- Result: 1628.89



-- Polynomial Evaluation
-- Evaluates a cubic polynomial using where for coefficients and powers.
evaluateCubic :: Floating a => a -> a -> a -> a -> a -> a
evaluateCubic a b c d x = result
  where
    term1 = a * x^3
    term2 = b * x^2
    term3 = c * x
    constant = d
    result = term1 + term2 + term3 + constant -- Usage: evaluateCubic 1 2 3 4 2 -- Result: 26.0



-- Calculating GCD with Euclidean Algorithm
-- Uses where in a recursive function to calculate the GCD.
gcdWhere :: Int -> Int -> Int
gcdWhere a b
  | b == 0 = a
  | otherwise = gcdWhere b remainder
  where
    remainder = a `mod` b -- Usage: gcdWhere 56 98 -- Result: 14