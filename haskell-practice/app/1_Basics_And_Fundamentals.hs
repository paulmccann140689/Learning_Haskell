-- Type signatures indicate what type of data a function can take and return. 
-- A type signature generally follows this format:

-- functionName :: Type

-- functionName: This is the name of the function or variable you are defining.

-- :: (Two Colons): This operator separates the name from the type, 
-- clearly indicating that what follows is the type information.

-- Type: This specifies the type of the function or variable, which could include complex types, 
-- type constructors, or even type variables.

-- Example of a Type Signature

add :: Int -> Int -> Int
add x y = x + y

-- add: This is the name of the function.
-- :: This indicates that what follows is the type signature of the function.
-- Int -> Int -> Int: This part describes the type of the function:
-- The first 'Int' indicates that the function takes an Int as its first argument.
-- The second 'Int' indicates that the function takes a second Int as its argument.
-- The final 'Int' indicates that the function returns an Int.