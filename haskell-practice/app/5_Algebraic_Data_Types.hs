-- Algebraic Data Types (ADTs) are fundamental constructs in Haskell that allow you to create 
-- complex types by combining simpler types. There are two primary forms of ADTs: Sum Types and Product Types.

-- Sum Types
-- Sum types allow a value to be one of several different types. 
-- Each variant of the sum type can carry different types and numbers of data. In Haskell, 
-- sum types are defined using the data keyword followed by a list of constructors. 

-- A common example is the Season type:
data Season = Spring | Summer | Autumn | Winter

-- In this example:
-- Season is the type name.
-- Spring, Summer, Autumn, and Winter are constructors. A value of type Season can be one of these four constructors.

-- Use Cases for Sum Types:
-- Representing states or options, like different seasons, types of vehicles, or HTTP status codes.
-- Enabling pattern matching for handling different cases cleanly.

-- Product Types
-- Product types combine multiple values into a single type, allowing you to group related data together. 
-- In Haskell, product types are also defined using the data keyword, but each constructor can take one or more arguments. 

-- An example of a product type is the UofGGrade type:
data UofGGrade = Grade Char Int

-- In this case:
-- UofGGrade is the type name.
-- Grade is the constructor that takes two parameters: a Char (representing a letter grade, e.g., 'A', 'B') 
-- and an Int (representing the numeric score).
-- Use Cases for Product Types:

-- Grouping related data together, such as representing a point in 2D space or a student's grade information.
-- Facilitating structured data where each component has its own type.


-- Sum Type: Colors
data Color = Red | Green | Blue
-- Purpose: Represents basic colors.
-- Use: Can be used in a graphics application to specify colors.


-- Product Type: Point in 2D Space
data Point = Point Float Float
-- Purpose: Represents a point with an x and y coordinate.
-- Use: Useful for geometrical calculations.


-- Sum Type: Direction
data Direction = North | South | East | West
-- Purpose: Represents cardinal directions.
-- Use: Can be used in navigation applications.


-- Sum Type: Vehicle Types
data Vehicle = Car String Int   -- Car with model name and year
             | Bike String      -- Bike with model name
             | Truck String Int -- Truck with model name and capacity
-- Purpose: Represents different types of vehicles.
-- Use: Can be utilized in a vehicle management system.


-- Product Type: Person Information
data Person = Person String Int  -- Name and age
-- Purpose: Represents a person's name and age.
-- Use: Useful in applications dealing with personal data.


-- Sum Type: HTTP Response
data HttpResponse = Success Int String  -- Status code and message
                  | NotFound String    -- Message
                  | Error String       -- Error message
-- Purpose: Represents different types of HTTP responses.
-- Use: Useful in web applications to handle different response types.


-- Sum Type: Expression
data Expr = Lit Int                     -- Literal integer
          | Add Expr Expr               -- Addition of two expressions
          | Multiply Expr Expr          -- Multiplication of two expressions
-- Purpose: Represents mathematical expressions.
-- Use: Can be used in an interpreter or compiler for evaluating or transforming mathematical expressions.


-- Product Type: Student Record
data Student = Student { name :: String
                       , age :: Int
                       , grades :: [UofGGrade]  -- List of grades
                       }
-- Purpose: Represents a student with their name, age, and grades.
-- Use: Useful in educational applications to manage student information.


-- Sum Type: File System Items
data FileSystemItem = File String Int  -- File with name and size
                    | Directory String [FileSystemItem]  -- Directory with name and list of items
-- Purpose: Represents items in a file system (files and directories).
-- Use: Useful for file management systems to model the structure of files and directories.

