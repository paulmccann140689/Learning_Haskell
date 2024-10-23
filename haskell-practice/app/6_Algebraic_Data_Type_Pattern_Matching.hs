-- Pattern matching allows you to decompose data structures. 
-- It works particularly well with Algebraic Data Types (ADTs), 
-- which provide a way to define types that can take on various forms (via sum types) 
-- or combine multiple values (via product types).

-- When you pattern match on an ADT, you can extract values from the data structure directly 
-- in the function's arguments. allowing you to handle different cases of a data type seamlessly.

-- Example Explanation
data Suit = Hearts | Diamonds | Clubs | Spades
data Card = King Suit | Queen Suit | Jack Suit | Ace Suit | Number Suit Int

showCard :: Card -> String
showCard (King _) = "K"
showCard (Queen _) = "Q"
showCard (Number _ n) = show n

-- Data Definitions:
-- Suit is a sum type representing the four suits of a deck of cards.
-- Card is an ADT that can represent various cards, including Kings, Queens, Jacks, Aces, and numbered cards.

-- Pattern Matching in showCard:
-- Each case of the function corresponds to a different constructor of the Card type.
-- When a Card is passed to showCard, it checks which constructor was used and extracts the relevant data.
-- The _ wildcard is used to ignore the Suit information since it’s not needed for the string representation of Kings and Queens.


-- Color Representation
data Color = Red | Green | Blue

showColor :: Color -> String
showColor Red = "Red"
showColor Green = "Green"
showColor Blue = "Blue"
-- Purpose: Converts a Color value to its string representation.
-- Pattern Matching: Each constructor of Color corresponds to a case in the function.


-- Simple Shapes
data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h
-- Purpose: Calculates the area of different shapes.
-- Pattern Matching: Decomposes Circle and Rectangle to extract their dimensions.


-- Days of the Week
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday = True
isWeekend _ = False
-- Purpose: Determines if a given Day is a weekend.
-- Pattern Matching: Directly matches Saturday and Sunday to return True.


-- Expression Types
data Expr = Lit Int | Add Expr Expr | Multiply Expr Expr

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Multiply e1 e2) = eval e1 * eval e2
-- Purpose: Evaluates arithmetic expressions.
-- Pattern Matching: Handles literals, addition, and multiplication.


-- Traffic Lights
data TrafficLight = Red2 | Yellow2 | Green2

lightAction :: TrafficLight -> String
lightAction Red2 = "Stop"
lightAction Yellow2 = "Caution"
lightAction Green2 = "Go"
-- Purpose: Determines the action based on the traffic light.
-- Pattern Matching: Each light color corresponds to a specific action.


-- Student Grades
data Grade = A | B | C | D | F

gradeMessage :: Grade -> String
gradeMessage A = "Excellent!"
gradeMessage B = "Well done!"
gradeMessage C = "You passed."
gradeMessage D = "You barely passed."
gradeMessage F = "You failed."
-- Purpose: Provides feedback based on a student's grade.
-- Pattern Matching: Each grade generates a specific message.


-- File System Structure
data FileSystemItem = File String Int           -- File with name and size
                    | Directory String [FileSystemItem]  -- Directory with items

totalSize :: FileSystemItem -> Int
totalSize (File _ size) = size
totalSize (Directory _ items) = sum (map totalSize items)
-- Purpose: Calculates the total size of files within a directory.
-- Pattern Matching: Differentiates between File and Directory to compute sizes recursively.


-- Game Entities
data GameEntity = Player String Int     -- Name and score
                | Enemy String Int      -- Name and health
                | Item String Int       -- Name and value

entityInfo :: GameEntity -> String
entityInfo (Player name score) = name ++ " has a score of " ++ show score
entityInfo (Enemy name health) = name ++ " has health: " ++ show health
entityInfo (Item name value) = name ++ " is worth " ++ show value ++ " points"
-- Purpose: Provides information about different game entities.
-- Pattern Matching: Differentiates between players, enemies, and items to return relevant information.