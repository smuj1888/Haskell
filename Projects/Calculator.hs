import System.Win32 (xBUTTON1)
-- multiply function
multiply :: Int -> Int -> Int
multiply a b = a * b

-- cube function
cube :: Num a => a -> a
cube x = x * x * x

--subtract function
subtractNums :: Int -> Int -> Int
subtractNums a b = a - b

--divide function
--division needs to be checked for dividing by zero so we use Maybe
divide :: Double -> Double -> Maybe Double
divide _ 0 = Nothing
divide a b = Just (a / b)

--Exponent function
power :: Int -> Int -> Int
power a b = a ^ b

--modulus function
modulus :: Int -> Int -> Int
modulus a b = a `mod` b

squareRoot :: Double -> Maybe Double
squareRoot x
    | x < 0 = Nothing
    | otherwise = Just (sqrt x)

main :: IO ()
main = do
-- IO actions and let bindings go here
    putStrLn "Enter the first number: "
    -- read the first number 
    input1 <- getLine
    let num1 = read input1 :: Int
    
    --read the second number and assign to a variable
    putStrLn "Enter the second number: "
    input2 <- getLine
    let num2 = read input2 :: Int

    -- call the multiply function
    let product = multiply num1 num2
    -- use show to convert the numbers to strings and concatenate them using ++
    putStrLn ("The product of " ++ show num1 ++ " and " ++ show num2 ++ " is " ++ show product)
    
    --we are only cubing the first number
    let cubed = cube num1
    putStrLn ("The cube of " ++ show num1 ++ " is " ++ show cubed)

    --subtract two numbers
    let diff = subtractNums num1 num2
    putStrLn ("The difference between " ++ show num1 ++ " and " ++ show num2 ++ " is " ++ show diff)

    --divide two numbers
    let num1Div = fromIntegral num1 :: Double
    let num2Div = fromIntegral num2 :: Double
    case divide num1Div num2Div of
        Nothing -> putStrLn "Cannot divide by zero"
        Just result -> putStrLn ("The division of " ++ show num1Div ++ " by " ++ show num2Div ++ " is " ++ show result)

    --exponent of two numbers
    let exp = power num1 num2
    putStrLn ("The exponent of " ++ show num1 ++ " to the power of " ++ show num2 ++ " is " ++ show exp)

    --modulus of two numbers
    let mod = modulus num1 num2
    putStrLn ("The modulus of " ++ show num1 ++ " and " ++ show num2 ++ " is " ++ show mod)

    --square root of the 2 numbers added together
    let sum = num1 + num2
    case squareRoot (fromIntegral sum :: Double) of
        Nothing -> putStrLn "Cannot square root a negative number"
        Just result -> putStrLn ("The square root of " ++ show sum ++ " is " ++ show result)
