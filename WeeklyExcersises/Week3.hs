import Data.Char
import Test.QuickCheck

import Data.Char (toUpper, isUpper, isAlpha, isAscii)


{--
 1. Write a function `twice` that calls a function f
 supplied as an argument two times on input x.
 Why must f have type (a->a) ?
--}

twice :: (a->a) -> a -> a
twice f x = f (f x)


{--
 2. Write a function `ntimes` that calls a function f
 supplied as an argument n times on input x (with n and
 x supplied as arguments too)
--}

ntimes :: (a->a) -> Int -> a -> a
-- f 0 x = x means that if n is 0, return x
ntimes f 0 x = x
-- otherwise, call f on x, and then call ntimes again with n-1
ntimes f n x =  ntimes f (n-1) (f x)

-- Quickcheck property for ntimes
prop_addm :: Int -> Bool
prop_addm = \m -> if m<0 then True else ntimes (+1) m 0 == m

-- ^^^^^ can you quickCheck this property?


{-- 3. write a function `exceptLast` which
  returns all the elements of a list except the
  final one, a little like the dual of the tail
  function.
  Use a recursive function definition.
  How many base cases do you need?
  What should you do with an empty list?
--}

exceptLast :: [a] -> [a]
-- base case: if the list is empty, return an empty list
exceptLast [] = []
-- base case: if the list has only one element, return an empty list
exceptLast [_] = []
-- otherwise, return the first element of the list, and then call exceptLast on the rest of the list
exceptLast (x:xs) = x : exceptLast xs

prop_exceptLast :: Eq a => [a] -> Bool
prop_exceptLast = \xs -> if length xs > 0
                         then (exceptLast xs == reverse (tail (reverse xs)))
                         else True

-- ^^^^^ can you quickCheck this property?

{--
  3. The Data.Char module has lots of useful helper function
  for characters. One of these is `toUpper`, which transforms a
  lower-case letter into its upper-case equivalent
  Write a function which transforms a list of strings (of mixed case)
  into a list of strings where all the letters are upper case.

  for instance, mkUpperCase [ "Once", "upon", "a", "time"]
  should evaluate to:
  [ "ONCE", "UPON", "A", "TIME"]
--}


-- The mkUpperCase function
mkUpperCase :: [String] -> [String]
mkUpperCase ws = map (map toUpper) ws

-- Property to test mkUpperCase, focusing only on ASCII alphabetic characters
prop_mkUpperCase :: [String] -> Property
prop_mkUpperCase xs =
    -- Filter out strings with non-ASCII characters
    all (all isAscii) xs ==>
    -- Check that for each string in the list, all alphabetic characters are uppercase after transformation
    all (all (\ch -> not (isAlpha ch) || isUpper ch)) (mkUpperCase xs)
    .&&. length xs === length (mkUpperCase xs)


{--
 4. Similarly, can you write a function that takes a single
    String and transforms it so the first character is
    upper case and all subsequent characters are lower case

    for instance `tidy "jEREMY"`
    should evaluate to "Jeremy"
--}

tidy :: String -> String
-- base case: if the string is empty, return an empty string/fix parse error on input ‘=’
tidy [] = []
tidy (x:xs) = toUpper x : map toLower xs

{--
  5. Tricky: Given a multi-word string,
  can you make the first letter of each word uppercase, with all
  other letters lowercase?
  So for example, given string "snow white and the seven dwarfs"
  your output should be "Snow White And The Seven Dwarfs"

  You may find the utility functions `words` and `unwords` helpful -
  like String split and join in Python :-)

--}

mkFirstLettersUpper :: String -> String
mkFirstLettersUpper sentence = unwords (map tidy (words sentence))

answer = mkFirstLettersUpper "snow white and the seven dwarfs"


{-- 6. Same as above, only words that are three characters or shorter
    should be all lowercase, i.e.
    mkFirstLettersUpper' "snow white and the seven dwarfs"
    evaluates to:
    "Snow White and the Seven Dwarfs"
--}

mkFirstLettersUpper' :: String -> String
mkFirstLettersUpper' sentence = unwords (map (\w -> if length w <= 3 then map toLower w else tidy w) (words sentence))

answer' = mkFirstLettersUpper' "snow white and the seven dwarfs"


{--
  7. Final part. Do you remember the binary trees datatype
  we looked at on Monday? Can you write some code to generate
  a String representing a depth first, pre-order traversal?

  So for tree1 below, the nodes should appear in order in the
  output String i.e. 1 2 3 4 5 
--}

data Tree = Leaf | Node Int Tree Tree

tree1 = Node 1 (Node 2 Leaf Leaf) (Node 3 (Node 4 Leaf Leaf) (Node 5 Leaf Leaf))

dfs_preorder :: Tree -> String
--if its a leaf, return an empty string
dfs_preorder Leaf = ""
--call dfs_preorder on the left and right nodes, and concatenate the value of the current node
dfs_preorder (Node value left right)=
    show value ++ " " ++ dfs_preorder left ++ dfs_preorder right

{--
 8. OK one more tiny bit for a bonus ... how about an inorder traversal, which
    would give "2 1 4 3 5" - I think ...
--}

dfs_inorder :: Tree -> String
--if its a leaf, return an empty string
dfs_inorder Leaf = ""
--call dfs_inorder on the left node, concatenate the value of the current node, and then call dfs_inorder on the right node
dfs_inorder (Node value left right)=
    dfs_inorder left ++ show value ++ " " ++ dfs_inorder right



main :: IO ()
main = do

    --QUESTION 1
    let result = twice (*2) 5
    putStrLn $ "twice (*2) 5 = " ++ show result


    --QUESTION 2
    putStrLn "Testing ntimes with prop_addm:"
    quickCheck prop_addm

    --QUESTION 3
    putStrLn "Testing exceptLast  with prop_exceptLast:"
    -- specifying type for QuickCheck
    quickCheck (prop_exceptLast :: [Int] -> Bool)


    --QUESTION 3
    putStrLn "Testing mkUpperCase with prop_mkUpperCase:"
    quickCheck prop_mkUpperCase

    --QUESTION 4
    putStrLn "Testing tidy:"
    putStrLn $ tidy "mARTIN"

    --QUESTION 5
    putStrLn answer

    --QUESTION 6
    putStrLn answer'

    --QUESTION 7
    putStrLn $ "Pre-order traversal of tree1: " ++ dfs_preorder tree1

    --QUESTION 8
    putStrLn $ "In-order traversal of tree1: " ++ dfs_inorder tree1