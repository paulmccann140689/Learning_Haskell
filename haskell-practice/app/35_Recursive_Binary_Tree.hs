-- A binary tree is a hierarchical data structure in which each node has at most two children, 
-- referred to as the left child and the right child. 
-- Each node in the binary tree can hold a value and links to two subtrees, which are themselves binary trees.

-- We can define a binary tree using an algebraic data type (ADT). 

-- data Tree = Leaf | Node Int Tree Tree

-- Leaf: 
-- This constructor represents an empty tree. In a binary tree context, it signifies a node that has no children.
-- Node:
-- This constructor represents a tree node that contains:
-- An integer value (Int), denoted as x.
-- A left subtree (Tree), which is another binary tree.
-- A right subtree (Tree), which is also another binary tree.

-- The structure can be visualized as:

--     Node
--    /    \
--  left   right

-- Sum of Values in a Binary Tree
-- The function treeSum calculates the sum of all integer values in a binary tree:

-- treeSum :: Tree -> Int
-- treeSum Leaf = 0
-- treeSum (Node x left right) = x + treeSum left + treeSum right

-- Base Case:
-- treeSum Leaf = 0: If the tree is empty (i.e., we hit a leaf node), the sum is 0. 
-- This serves as the termination condition for the recursion.

-- Recursive Case:
-- treeSum (Node x left right) = x + treeSum left + treeSum right:

-- When encountering a Node, we:
-- Take the value of the current node (x).
-- Recursively call treeSum on the left subtree to get its sum.
-- Recursively call treeSum on the right subtree to get its sum.

-- The total sum at that node is the sum of the current node’s value and the sums of the left and right subtrees.

-- Consider the following binary tree:

--         5
--        / \
--       3   8
--      / \
--     1   4

-- exampleTree :: Tree
-- exampleTree = Node 5 
--                    (Node 3 
--                          (Node 1 Leaf Leaf) 
--                          (Node 4 Leaf Leaf)) 
--                    (Node 8 Leaf Leaf)

-- -- Using treeSum
-- totalSum = treeSum exampleTree -- totalSum will be 5 + 3 + 1 + 4 + 8 = 21

-- Inverting a binary tree means swapping the left and right children of all nodes in the tree. 
-- invert :: Tree -> Tree
-- invert Leaf = Leaf
-- invert (Node x t1 t2) = Node x (invert t2) (invert t1)

-- Base Case:
-- invert Leaf = Leaf: If the tree is empty (i.e., we hit a leaf), it remains unchanged.

-- Recursive Case:
-- invert (Node x t1 t2) = Node x (invert t2) (invert t1):
-- When encountering a Node, we:
-- Keep the value of the current node (x).
-- Recursively call invert on the right subtree (t2), which becomes the new left child.
-- Recursively call invert on the left subtree (t1), which becomes the new right child.

-- The result is a new node with the value x, where its left and right children have been swapped.

-- Consider the same binary tree:
--         5
--        / \
--       3   8
--      / \
--     1   4

-- Inverting the example tree
-- invertedTree = invert exampleTree

-- The structure of invertedTree would be:
--         5
--        / \
--       8   3
--            / \
--           4   1