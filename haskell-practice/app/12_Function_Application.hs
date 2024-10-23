-- Function application is the process of providing arguments to a function and obtaining the result. 
-- In Haskell, function application is central, as it’s the main way to invoke functions and calculate values. 
-- Function application in Haskell is very succinct and has left associativity, 
-- meaning multiple functions and arguments can be applied in sequence, without excessive parentheses.

-- Function application has specific rules and characteristics:

-- Syntax: In Haskell, a function is applied by writing the function name followed by the argument, 
-- separated by a space (e.g., f x instead of f(x) as in many other languages).

-- Precedence: Function application has the highest precedence and is left-associative, 
-- meaning that in expressions without parentheses, function applications are evaluated from left to right.

-- Parentheses for Clarity: When mixing function applications with operations (such as arithmetic), 
-- parentheses are often required to clarify the order of application. For instance, 
-- addOne 5 * 2 and addOne (5 * 2) yield different results 
-- because parentheses control which operation (function application or multiplication) is evaluated first.


-- Simple Addition with Function Application:
addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y

functionApplicationExample1 :: Int
functionApplicationExample1 = addTwoNumbers 3 4 -- Result: 7




-- Using Function Application with Multiplication:
multiplyBy3 :: Int -> Int
multiplyBy3 x = x * 3

functionApplicationExample2 :: Int
functionApplicationExample2 = multiplyBy3 10 -- Result: 30




-- Using Function Application and Order of Operations:
doubleThenAddFive :: Int -> Int
doubleThenAddFive x = x * 2 + 5

functionApplicationExample3 :: Int
functionApplicationExample3 = doubleThenAddFive 3 -- Result: 11




-- Composing Functions with Application:
addOne :: Int -> Int
addOne x = x + 1

double :: Int -> Int
double x = x * 2

functionApplicationExample4 :: Int
functionApplicationExample4 = addOne (double 4) -- Result: 9




-- Using Function Application with Division:
halfThenSubtractThree :: Double -> Double
halfThenSubtractThree x = (x / 2) - 3

functionApplicationExample5 :: Double
functionApplicationExample5 = halfThenSubtractThree 10.0 -- Result: 2.0





-- Combining Multiple Functions:
addTen :: Int -> Int
addTen x = x + 10

square :: Int -> Int
square x = x * x

functionApplicationExample6 :: Int
functionApplicationExample6 = square (addTen 3) -- Result: 169




-- Nested Function Application:
subtractOne :: Int -> Int
subtractOne x = x - 1

cube :: Int -> Int
cube x = x * x * x

functionApplicationExample7 :: Int
functionApplicationExample7 = subtractOne (cube (addTwoNumbers 3 4)) -- Result: 342




-- Using Function Application in a Conditional Expression:
isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

checkEvenAddFive :: Int -> Int
checkEvenAddFive x = if isEven x then x + 5 else x

functionApplicationExample8 :: Int
functionApplicationExample8 = checkEvenAddFive (double (subtractOne 9)) -- Result: 16





-- Recursive Function with Function Application:
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

functionApplicationExample9 :: Int
functionApplicationExample9 = factorial (addOne 4) -- Result: 120