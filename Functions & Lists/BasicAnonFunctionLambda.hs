-- In haskell you can create anonymous lambda function using the backslash \ Thes are function without names and are often used for short, simple operations
greet :: String -> String
greet = \name -> "Hello, " ++ name

addFive :: Int -> Int
addFive = \x -> x + 5


-- function that takes a tuple of two numbers and adds them
addTuple :: (Int, Int) -> Int
addTuple = \(n1, n2) -> n1 + n2

-- Curried functions are functions that take multiple arguments one at a time.
addCurried :: Int -> Int -> Int
-- takes n1 and returns a function that takes n2 and allows you to supply arguments one at a time
addCurried = \n1 -> (\n2 -> n1 + n2)

-- Simplified curried function with a clear type signature
multiply :: Int -> Int -> Int
multiply x y = x * y


main :: IO ()

main = do
    -- use the greet function with a name
    let message = greet " Alice "
    putStrLn message

    let result = addFive 10
    print result

    -- takes a tuple input and returns the sum of the two numbers
    let result2 = addTuple (5, 7)
    print result2

    let result3 = addCurried 5 7
    print result3

    let result4 = multiply 3 4
    print result4  

-- Anonymous functions take one argument and return a String. They are useful for short, simple operations that are not worth naming.
-- first class functions in haskell can be let-bound to variables like greet which makes it reusable
