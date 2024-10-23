-- In Haskell, let bindings allow you to define local variables and expressions within a specific scope, 
-- such as within a function. A let binding introduces names (variables or functions) and assigns them values or expressions,
-- making complex or repetitive calculations easier to handle. 
-- Unlike a where clause, which also defines local variables, let bindings can be used in expressions and nested structures, 
-- offering more flexible usage.

-- The basic syntax of a let binding in Haskell is:

-- let <bindings> in <expression>

-- <bindings> is where you define variables or functions.
-- <expression> is where the result is calculated using the bound variables.


-- Key Characteristics of Let Bindings
-- Scope: Variables defined in a let binding are only accessible in the in expression that follows the let section. 
-- This scoping helps keep variables contained and avoids accidental conflicts with names elsewhere.

-- Multiple Bindings: You can define multiple bindings at once by stacking them within a let block, 
-- separated by newline or semicolons.

-- Immutability: Bindings in Haskell are immutable, so once defined, they cannot be reassigned. 
-- If you need a new value, you’ll define a new name for it.

-- Nesting: let bindings can be nested, meaning you can define inner let blocks within outer ones, 
-- allowing complex computations to be broken down into manageable parts.

-- Example Breakdown
-- In the example below, quadraticRootsLet, a let binding is used to define intermediate values 
-- for calculating the roots of a quadratic equation:

-- quadraticRootsLet :: Floating a => a -> a -> a -> [a]
-- quadraticRootsLet a b c =
--   let det2 = b * b - 4 * a * c
--       det = sqrt det2
--   in [(-b + det) / (2 * a), (-b - det) / (2 * a)]

-- Step-by-step
-- det2 is assigned the discriminant formula b * b - 4 * a * c.
-- det is the square root of det2.
-- The values det2 and det are then used in the expression following in, 
-- where they simplify the calculation of the two roots without duplicating code.



-- Simple Arithmetic Calculation
-- Uses let to break down a calculation into simpler steps.
addWithLet :: Int
addWithLet =
  let x = 5
      y = 10
  in x + y -- Result: 15



-- Concatenating Strings
-- Concatenates two strings with let bindings for clarity.
greetLet :: String
greetLet =
  let firstName = "Alice"
      lastName = "Smith"
  in "Hello, " ++ firstName ++ " " ++ lastName -- Result: "Hello, Alice Smith"



-- Calculating Area of a Rectangle
-- Defines variables for width and height before computing the area.
areaLet :: Int
areaLet =
  let width = 4
      height = 5
  in width * height -- Result: 20



-- Finding the Hypotenuse of a Right Triangle
-- Uses let to define squared sides before calculating the hypotenuse.
hypotenuseLet :: Floating a => a -> a -> a
hypotenuseLet a b =
  let a2 = a * a
      b2 = b * b
  in sqrt (a2 + b2) -- Usage: hypotenuseLet 3 4 -- Result: 5.0



-- Combining Lists
-- Uses let to define intermediate lists before concatenating them.
combineListsLet :: [Int]
combineListsLet =
  let list1 = [1, 2, 3]
      list2 = [4, 5, 6]
  in list1 ++ list2 -- Result: [1,2,3,4,5,6]



-- Calculating the Average of Three Numbers
-- Uses let to define the sum and then compute the average.
averageLet :: Fractional a => a -> a -> a -> a
averageLet x y z =
  let sum = x + y + z
      count = 3
  in sum / fromIntegral count -- Usage: averageLet 5 10 15 -- Result: 10.0



-- Calculating Compound Interest
-- Uses let to define intermediate values in a compound interest formula.
compoundInterest :: Floating a => a -> a -> Int -> a
compoundInterest principal rate years =
  let factor = 1 + rate
      finalAmount = principal * (factor ^ years)
  in finalAmount -- Usage: compoundInterest 1000 0.05 10 -- Result: 1628.894...



-- Filtering and Summing Even Numbers
-- Uses let to filter even numbers from a list and sum them up.
sumEvensLet :: [Int] -> Int
sumEvensLet nums =
  let evens = filter even nums
      total = sum evens
  in total -- Usage: sumEvensLet [1,2,3,4,5,6] -- Result: 12



-- Fibonacci with Let Bindings
-- Computes a specific Fibonacci number using recursion and let bindings.
fibonacciLet :: Int -> Int
fibonacciLet n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise =
      let fibPrev1 = fibonacciLet (n - 1)
          fibPrev2 = fibonacciLet (n - 2)
      in fibPrev1 + fibPrev2 -- Usage: fibonacciLet 6 -- Result: 8