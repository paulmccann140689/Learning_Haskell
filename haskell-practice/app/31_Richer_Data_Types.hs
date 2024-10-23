-- Richer data types refer to complex types that not only combine multiple values 
-- but also express more intricate relationships and behaviors. 
-- These data types can represent various forms of data, encapsulating both the data itself 
-- and associated behaviors through type constructors and algebraic data types (ADTs).

-- The idea behind richer data types is to model more complex domains in a type-safe way. 
-- They can represent hierarchical structures, enable pattern matching, and facilitate encapsulation of behavior, 
-- all while keeping the type system expressive and robust.

-- Example Explanation

data Suit = Hearts | Diamonds | Clubs | Spades
data Card = King Suit | Queen Suit | Jack Suit | Ace Suit | Number Suit Int

-- Suit:
-- Type: Suit is a sum type (or enumerated type) that represents the four suits of a standard deck of playing cards.
-- Constructors: Hearts, Diamonds, Clubs, and Spades are the constructors that define the possible values of this type.

-- Card:
-- Type: Card is a richer type that uses the Suit type to define the various cards in the deck.
-- Constructors:
-- King Suit: Represents a King card of a specific suit.
-- Queen Suit: Represents a Queen card of a specific suit.
-- Jack Suit: Represents a Jack card of a specific suit.
-- Ace Suit: Represents an Ace card of a specific suit.
-- Number Suit Int: Represents numbered cards (2-10) of a specific suit, with the Int representing the card's value.


-- RGB Color Representation
data Color = RGB Int Int Int  -- Red, Green, Blue components
-- Purpose: Represents a color using its red, green, and blue components.
-- Use: Useful in graphics programming to define colors.


-- Simple Geometric Shapes
data Shape = Circle Float       -- Radius
           | Rectangle Float Float -- Width and Height
-- Purpose: Represents basic geometric shapes with appropriate dimensions.
-- Use: Useful for calculating areas or performing transformations on shapes.


-- Days of the Week
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
-- Purpose: Represents the days of the week.
-- Use: Useful in scheduling applications or calendar functionalities.


-- Binary Tree
data Tree a = Leaf a               -- Leaf node with a value
            | Node (Tree a) (Tree a)  -- Node with left and right subtrees
-- Purpose: Represents a binary tree, allowing recursive structures.
-- Use: Useful for implementing various tree algorithms, such as search or traversal.


-- Expression Types
data Expr = Lit Int                      -- Literal integer
          | Add Expr Expr                -- Addition of two expressions
          | Multiply Expr Expr           -- Multiplication of two expressions
-- Purpose: Represents arithmetic expressions.
-- Use: Useful in evaluators for mathematical expressions.


-- User Roles in a System
data Role = Admin | Editor | Viewer
data User = User { username :: String
                 , role :: Role
                 }
-- Purpose: Represents a user with a username and a role.
-- Use: Useful in applications requiring user management and permissions.



-- File System Structure
data FileSystemItem = File String Int           -- File with name and size
                    | Directory String [FileSystemItem]  -- Directory with name and list of items
-- Purpose: Represents a file system structure with files and directories.
-- Use: Useful for building file management systems.


-- Graph Representation
data Graph a = Empty                     -- An empty graph
             | Edge a a                   -- An edge between two vertices
             | Vertex a [Graph a]        -- A vertex with a list of connected vertices
-- Purpose: Represents a graph with vertices and edges.
-- Use: Useful for implementing algorithms like depth-first search or Dijkstra’s algorithm.


-- Music Playlist
data Genre = Pop | Rock | Jazz | Classical
data Song = Song { title :: String
                 , artist :: String
                 , genre :: Genre
                 , duration :: Int  -- Duration in seconds
                 }
data Playlist = Playlist { name :: String
                         , songs :: [Song]
                         }
-- Purpose: Represents songs with their metadata and a playlist of songs.
