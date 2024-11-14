  -- Recursion (including Mutual Recursion / Tail Recursion)
-- Recursion is a fundamental concept in functional programming where a function calls itself.
-- It consists of two parts:
-- 1. Base Case: This is the simplest instance where recursion terminates.
-- 2. Recursive Case: A case that calls the same function with a smaller or simpler argument to work towards the base case.

-- Mutual Recursion:
-- Mutual recursion involves two or more functions calling each other.
-- Example:
evenCheck :: Int -> Bool
evenCheck 0 = True
evenCheck n = oddCheck (n-1)

oddCheck :: Int -> Bool
oddCheck 0 = False
oddCheck n = evenCheck (n-1)

-- Tail Recursion:
-- Tail recursion means the recursive call is the last operation performed by the function.
-- Tail recursion is optimized by Haskell's compiler to avoid consuming extra stack space.
factorialTail :: Int -> Int
factorialTail n = factHelper n 1
  where
    factHelper 0 acc = acc        -- Base case: return the accumulated value
    factHelper n acc = factHelper (n - 1) (n * acc) -- Tail recursive call

-- Example usage of factorial
-- factorialTail 5 == 120

-- Algebraic Data Types: Definition, Pattern Matching
-- Algebraic Data Types (ADTs) allow us to create complex data structures by combining simpler types.
-- Example of defining a custom data type:
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show)

-- Pattern Matching with Algebraic Data Types:
-- We can use pattern matching to handle different data constructors.
weekendOrWeekday :: Day -> String
weekendOrWeekday Saturday = "It's the weekend!"
weekendOrWeekday Sunday = "It's the weekend!"
weekendOrWeekday _ = "It's a weekday."

-- Example usage:
-- weekendOrWeekday Saturday == "It's the weekend!"

-- Case Expressions:
-- Case expressions are similar to switch statements in imperative languages.
-- They allow branching logic by matching values to specific cases.
describeNumber :: Int -> String
describeNumber num = case num of
  1 -> "one"
  2 -> "two"
  3 -> "three"
  _ -> "something else" -- Default case for any value not covered above

-- Guards:3
-- Guards allow selecting different branches based on Boolean conditions.
-- It provides a cleaner alternative to if-else chains.
gradeFromGPA :: Int -> String
gradeFromGPA gpa
  | gpa >= 18 = "A"       -- Grade A for GPA 18 or higher
  | gpa >= 15 = "B"       -- Grade B for GPA 15 or higher
  | gpa >= 12 = "C"       -- Grade C for GPA 12 or higher
  | otherwise = "below C" -- Final case matches anything else

-- Recursion: Lists are Inductively-Defined Data Structures
-- Lists in Haskell are defined recursively:
-- - [] represents an empty list.
-- - (:) adds an element to the front of the list.
-- Example of constructing a list recursively:
buildList :: Int -> [Int]
buildList 0 = []
buildList n = n : buildList (n - 1)

-- Example usage:
-- buildList 3 == [3, 2, 1]

-- Example: Factorial (Non-Tail Recursive Version)
-- A simple example of recursion on integers.
-- Ensures that there is a base case and a negativity check to avoid incorrect results.
factorialSimple :: Int -> Int
factorialSimple 0 = 1
factorialSimple n
  | n < 0 = error "Negative value not allowed"
  | otherwise = n * factorialSimple (n - 1)

-- Example usage:
-- factorialSimple 3 == 6

-- Tail Recursion
-- Tail recursion allows more efficient recursion by making the recursive call the final operation.
-- Haskell uses tail-call optimization to manage stack usage efficiently.
tailSum :: [Int] -> Int
tailSum xs = tailSumHelper xs 0
  where
    tailSumHelper [] acc = acc          -- Base case: return accumulated value
    tailSumHelper (x:xs) acc = tailSumHelper xs (acc + x) -- Tail recursive call

-- Mutual Recursion
-- Functions that call each other are mutually recursive.
-- This allows breaking complex logic into simpler parts.
isEven :: Int -> Bool
isEven 0 = True
isEven n = isOdd (n - 1)

isOdd :: Int -> Bool
isOdd 0 = False
isOdd n = isEven (n - 1)

-- Example usage:
-- isEven 4 == True
-- isOdd 3 == True

-- Defining Data Types: Product Types
-- Product types combine multiple values together.
-- Example:
data Grade = Grade Char Int deriving (Show)

-- Pattern Matching
-- Pattern matching helps in destructuring data types to access the underlying data.
-- It can be done with different constructors and by using the wildcard symbol.
showGrade :: Grade -> String
showGrade (Grade letter value) = "Grade: " ++ [letter] ++ " with value: " ++ show value

-- Example usage:
-- showGrade (Grade 'A' 90) == "Grade: A with value: 90"

-- Recursive Data Types: Binary Trees
-- Binary trees are recursively defined data structures.
data Tree = Leaf | Node Int Tree Tree deriving (Show)

-- Example of calculating the sum of all nodes in a tree.
treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node value left right) = value + treeSum left + treeSum right

-- Example usage:
-- treeSum (Node 5 (Node 3 Leaf Leaf) (Node 2 Leaf Leaf)) == 10

-- Parameterised Data Types: Binary Trees
-- Parameterised data types use type variables so they can contain values of any type.
data BinaryTree a = Empty | BTNode a (BinaryTree a) (BinaryTree a) deriving (Show)

-- Inverting a binary tree:
invert :: BinaryTree a -> BinaryTree a
invert Empty = Empty
invert (BTNode value left right) = BTNode value (invert right) (invert left)

-- Example usage:
-- invert (BTNode 1 (BTNode 2 Empty Empty) (BTNode 3 Empty Empty)) == BTNode 1 (BTNode 3 Empty Empty) (BTNode 2 Empty Empty)

-- The Maybe Type: Handling Potentially Failing Computations
-- The Maybe type represents computations that may fail.
-- It either contains a value (Just a) or no value at all (Nothing).
safeHead :: [a] -> Maybe a
safeHead [] = Nothing        -- Empty list returns Nothing
safeHead (x:_) = Just x      -- Non-empty list returns Just the first element

-- Example usage:
-- safeHead [1, 2, 3] == Just 1
-- safeHead [] == Nothing

main :: IO ()
main = do
  -- Recursion Examples
  putStrLn "== Recursion: Factorial Example =="
  print $ factorialTail 5 -- Expected output: 120
  
  putStrLn "\n== Mutual Recursion: Even and Odd Check =="
  print $ isEven 4    -- Expected output: True
  print $ isOdd 3     -- Expected output: True

  putStrLn "\n== Tail Recursion: Sum Example =="
  print $ tailSum [1, 2, 3, 4] -- Expected output: 10

  -- Algebraic Data Types and Pattern Matching Examples
  putStrLn "\n== Algebraic Data Type: Day Example =="
  print $ weekendOrWeekday Saturday -- Expected output: "It's the weekend!"

  putStrLn "\n== Case Expression Example =="
  print $ describeNumber 2  -- Expected output: "two"
  print $ describeNumber 5  -- Expected output: "something else"

  -- Guards Example
  putStrLn "\n== Guards Example: Grade From GPA =="
  print $ gradeFromGPA 16   -- Expected output: "B"
  print $ gradeFromGPA 10   -- Expected output: "below C"

  -- Recursion: List Building Example
  putStrLn "\n== Recursion: Build List =="
  print $ buildList 3       -- Expected output: [3, 2, 1]

  -- Product Type and Pattern Matching Example
  putStrLn "\n== Defining Data Type: Product Type =="
  print $ showGrade (Grade 'A' 85) -- Expected output: "Grade: A with value: 85"

  -- Recursive Data Types: Binary Tree Example
  putStrLn "\n== Recursive Data Types: Binary Tree Sum =="
  let myTree = Node 5 (Node 3 Leaf Leaf) (Node 2 Leaf Leaf)
  print $ treeSum myTree    -- Expected output: 10

  -- Parameterised Data Types: Binary Tree Example
  putStrLn "\n== Parameterised Data Types: Invert Binary Tree =="
  let myBinaryTree = BTNode 1 (BTNode 2 Empty Empty) (BTNode 3 Empty Empty)
  print $ invert myBinaryTree -- Expected output: BTNode 1 (BTNode 3 Empty Empty) (BTNode 2 Empty Empty)

  -- Maybe Type Example
  putStrLn "\n== The Maybe Type Example: Safe Head =="
  print $ safeHead [1, 2, 3] -- Expected output: Just 1
  -- Adding explicit type annotations for the list to remove ambiguity
  print $ safeHead ([] :: [Int])        -- Expected output: Nothing

