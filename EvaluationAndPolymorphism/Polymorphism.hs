-- Functional Programming: Polymorphism

-- Polymorphism Overview:
-- - Polymorphism comes from Greek, meaning "many forms".
-- - It refers to functions operating on values of various types.
-- - Enables code reuse and helps reason about program behavior.

-- Two Main Types of Polymorphism:
-- 1. Parametric Polymorphism:
--    - Functions operate on the shape of arguments, not the data itself.
--    - Uses type variables.
-- 2. Ad-hoc Polymorphism:
--    - Similar to method overriding in languages like Java.
--    - Same function name, different implementations for different types.
--    - Achieved in Haskell using **typeclasses**.

-- Parametric Polymorphism: 
-- - Functions that work for any type.

-- Some Classic Examples of Parametric Polymorphism:
id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

-- Type variables `a` and `b` will be specialized to concrete types
-- when the function is called on concrete values. The behavior of 
-- functions like `map` and `filter` does not depend on the specific 
-- types, just the structure of the data.

-- Another Example: `length`, `folds`
length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

-- Functions of Type `a -> b -> a`
-- There is precisely one function with the type `a -> b -> a`: 
const :: a -> b -> a
const x _ = x

-- This is a consequence of **parametricity**.
-- (For more, see "Theorems for Free!")

-- Defining Our Own Polymorphic Functions:
-- Example: Duplicate a value and put it in a list.
dapial :: a -> [a]
dapial x = [x, x]

-- Example: Extract the last element from a triple.
takeLast :: (a, b, c) -> c
takeLast (_, _, z) = z

-- Note: Both examples only manipulate the variables based on their structure,
-- without using any information about their specific types.

-- Polymorphic Data Types:
-- We can also use type variables within **data type declarations**.

-- A polymorphic binary tree data structure:
data Tree a = 
    Leaf a 
  | Node (Tree a) (Tree a)

-- Once specialized, the tree will have a fixed concrete type:
-- Examples of Tree Int and Tree String:
-- Node (Leaf 5) (Leaf 10) :: Tree Int
-- Node (Node (Leaf "Hello") (Leaf "World")) (Leaf "!") :: Tree String

-- Functions for Polymorphic Container Types:
-- We can define functions like `treeMap`, which maps over the structure of a Tree.
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x) = Leaf (f x)
treeMap f (Node t1 t2) = Node (treeMap f t1) (treeMap f t2)

-- Looking ahead: `treeMap` is an instance of a **functor** for trees.