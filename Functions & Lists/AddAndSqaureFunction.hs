
-- Define the add fucntion
add :: Int -> Int -> Int
add a b = a + b

-- Define the square function
square :: Num a => a -> a
square x = x * x


-- define the main function
main :: IO ()
main = do
    --then use the add function and print the results
    let result = add 3 5
    putStrLn ("The sum of 3 + 5 is: " ++ show result)

    let squared = square 4 
    putStrLn ("The square of 4 is: " ++ show squared)