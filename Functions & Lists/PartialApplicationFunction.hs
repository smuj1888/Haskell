-- Define a function that adds two numbers
add :: Int -> Int -> Int
add x y = x + y

-- Partially apply the function by supplying only one argument
addFive :: Int -> Int
addFive = add 5  -- This creates a new function that adds 5 to its argument

main :: IO ()
main = do
    -- Use the partially applied function
    let result = addFive 10
    print result  -- Output: 15

--By applying only one argument to add, we create addFive, which is a specialized version of add.