-- Function composition : composing two functions
addOne :: Int -> Int
addOne x = x + 1

double :: Int -> Int
double x = x * 2

--compose addOne and double to create a new function
addOneThenDouble :: Int -> Int
addOneThenDouble = double . addOne

main :: IO ()
main = do
    putStrLn ("The result of addOneThenDouble is: " ++ show (addOneThenDouble 3))