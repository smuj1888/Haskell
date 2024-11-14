
-- Currying example, functions with multiple arguments are nested functions
add :: Int -> Int -> Int
add x y = x + y

-- Partial application example: we can partially apply 'add' to create a new function
addFive :: Int -> Int
addFive = add 5 

main :: IO ()
main = do
    --Fully apply both arguments
    putStrLn ( "The result of add 5 10 is: " ++ show (add 5 10))

    -- Partial application
    putStrLn ("The result of addFive 10 is: " ++ show (addFive 10))

