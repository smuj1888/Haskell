{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

--A Tree is either a Leaf or a Node with an integer and two subtrees left and right
data Tree = Leaf | Node Int Tree Tree
    --allow the Tree to be printed
    deriving (Show)  



--INSERT VALUES 
insertValue :: Int -> Tree -> Tree
--if the tree is a leaf, create a new node with the value x
insertValue x Leaf = Node x Leaf Leaf

insertValue x (Node value left right) 
    -- if x is less than or equal to value then insert into the left subtree
    | x <= value = Node value (insertValue x left) right
    -- otherwise it goes in to the right subtree
    | otherwise = Node value left (insertValue x right)


--BUILD THE TREE
buildTree :: [Int] -> Tree
-- start with an empty tree (Leaf) and insert each number from the list
buildTree = foldl (flip insertValue) Leaf


--PRE ORDER TRAVERSAL (visit root, then left, then right)
dfs_Preorder :: Tree -> String
--base case if the tree is a leaf then return an empty string
dfs_Preorder Leaf = ""
--concatenate current nodes value, left traversal and right traversal
dfs_Preorder (Node value left right) = 
    show value ++ " " ++ dfs_Preorder left ++ dfs_Preorder right


--IN ORDER TRAVERSAL (visit left, then root, then right)
dfs_Inorder :: Tree -> String
--base case if the tree is a leaf then return an empty string
dfs_Inorder Leaf = ""
--concatenate left traversal, current nodes value then right traversal
dfs_Inorder (Node value left right) =
    dfs_Inorder left ++ show value ++ " " ++ dfs_Inorder right


main :: IO ()
main = do
    --get user input as a string
    putStrLn "Enter a sequence of numbers (space-separated):"  
    input <- getLine
    --split the input string into words, convert to integers
    let numbers = map read (words input) :: [Int]  

    --build the binary tree using the list of numbers
    let tree = buildTree numbers  
    -- get user input fro the traversal type
    putStrLn "Choose traversal type: (1) Pre-order (2) In-order"  
    choice <- getLine 
    
    case choice of
        --if the user selects 1, display the pre-order traversal
        -- Print the result of the pre-order traversal
        "1" -> do
            putStrLn "Pre-order traversal:"  
            putStrLn $ dfs_Preorder tree  

        --if the user selects 2, display the in-order traversal
        -- Print the result of the in-order traversal
        "2" -> do
            putStrLn "In-order traversal:"  
            putStrLn $ dfs_Inorder tree  
        _ -> putStrLn "Invalid choice! Please choose 1 or 2." 
