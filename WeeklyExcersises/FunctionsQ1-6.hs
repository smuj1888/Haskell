--QUESTION 1
--write a function that takes two ints as arguments and returns the max value
max2 :: Int -> Int -> Int
max2 x y = if x > y then x else y

--QUESTION 2
-- to compare the 3 numbers we get the max of the first 2 and then compare that to z 
max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 (max2 x y) z

--QUESTION3
-- Function that converts an Int to a String
intToString :: Int -> String
intToString x = show x  -- 'show' converts any type to a String

-- Function that converts a String to a Bool (let's assume non-empty strings are True)
stringToBool :: String -> Bool
stringToBool str = not (null str)  -- 'null' checks if the string is empty

f :: (Int -> String) -> (String -> Bool) -> (Int -> Bool)
--Your function should take two arguments (the two functions), and then return a new function that does the composition
--g is the first function (int-> String)
--h is the second (String -> bool)
f g h = \x -> h (g x)


--QUESTION 4

intToBool :: Int -> Bool
intToBool x = x `mod` 2 == 0 -- this will be true if x is even and false if odd

boolToString :: Bool -> String
boolToString True = "Even"
boolToString False = "Odd"

g :: (Int -> Bool) -> (Bool -> String) -> Int -> String
-- f is the first function (int -> bool)
-- h is the second function (bool -> string)
-- x is the Int input
-- (f x) applies f to the Int and produces a bool 
-- h (f x) applies h to the bool and produces a string 
g f h x  = h (f x) 


-- QUESTION 5
-- twice function applies functon f to an integer x twice
twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

--simple function to multiply a number by 2
double :: Int -> Int
double x = 2 * x


-- QUESTION 6
gravity :: Double -> Double -> Double -> Double
gravity m1 m2 d = 
    let g = 6.67e-11 -- universal gravitation constant
        dSqaured = d ** 2  -- square of the distance
    in (g* m1 * m2)/ dSqaured 

main :: IO ()
main = do

    -- QUESTION1
    let resultMax = max2 20 5
    print resultMax

    --QUESTION2
    let resultMax3 = max3 10 20 30
    print resultMax3

    --QUESTION 3
    let composedFunction = f intToString stringToBool  -- Compose intToString and stringToBool
    let testValue = 42  -- Example value to test the composed function
    let result = composedFunction testValue  -- Apply the composed function to the test value
    putStrLn ("The result of applying the composed function to " ++ show testValue ++ " is: " ++ show result)


    --QUESTION 4
    -- Test the g function with intToBool and boolToString
    print (g intToBool boolToString 4)  -- Output: "Even"
    print (g intToBool boolToString 7)  -- Output: "Odd#

    --QUESTION 5
    let number = 3
    let result = twice double number
    putStrLn ("Applying 'double' twice to " ++ show number ++ " gives: " ++ show result)

    --QUESTION 6
    let m1 = 5.972e24  -- Example mass (Earth's mass in kg)
    let m2 = 7.35e22   -- Example mass (Moon's mass in kg)
    let d = 3.844e8    -- Distance between Earth and Moon in meters
    let force = gravity m1 m2 d
    putStrLn ("The gravitational force between the masses is: " ++ show force ++ " N")


