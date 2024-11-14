-- basic equation assigning a value to a name
favouriteNumber :: Int
favouriteNumber = 16

-- define a simple function using a lambda expression ( anonymous functin )
addLambda :: Int -> Int -> Int
-- anonymous function that takes two arguments and returns their sum
addLambda = \x -> \y -> x + y

-- more concise way to write the same function
add :: Int -> Int -> Int
add x y = x + y



main :: IO ()
main = do
    -- use the favouriteNumber 
    print favouriteNumber

    let resultLambda = addLambda 3 4
    print resultLambda

    let resultAdd = add 10 15
    print resultAdd