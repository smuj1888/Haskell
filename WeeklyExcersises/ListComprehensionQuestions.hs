-- QUESTION 1
divisibleBy3 :: [Int]
--generate a list of numbers between 1 and 30 and only include ones that are divise by 3 using mod
divisibleBy3 = [x | x <- [1..30], x `mod` 3 == 0]

-- QUESTION 2
triangles :: Int -> [Int]
--generate a list of the first n triangle numbers using the formula n(n+1)/2
triangles n = [ (i * (i + 1)) `div` 2 | i <- [1..n] ]


-- QUESTION 3
isPrime :: Int -> Bool
-- The list comprehension [ y | y <- [2..x-1], x mod y == 0 ] generates all the divisors of x in the range 2 to x-1.
-- if there are no divisors, then the number is prime, so the list should be empty.
isPrime x = null [ y | y <- [2..x-1], x `mod` y == 0]

--take an int and return a list of all prime numbers between 2 and n
--the function includes only numbers that are prime using the isPrime function
primes :: Int -> [Int]
primes n =[x | x <- [2..n], isPrime x]


--QUESTION 4
--flatten a list of lists into a single list
flatten :: [[a]] -> [a]
-- x <- xs iterates over each sublist x in the outer list xs
-- y <- x iterates over each element y in the sublist x
-- the list comprehension [y | x <- xs, y <- x] generates a flat list of all the elements in the sublists of xs.
flatten xs = [y | x <- xs, y <- x]


main :: IO ()

main = do
    -- QUESTION 1
    putStrLn ("Numbers between 1 and 30 divisible by 3: " ++ show divisibleBy3)

    -- QUESTION 2
    putStrLn ("Triangles: " ++ show (triangles 5))

    -- QUESTION 3
    let n = 30  
    putStrLn ("Prime numbers: " ++ show (primes n))

    -- QUESTION 4
    let nestedList = [[1,2,3], [4,5,6], [7,8,9]]
    putStrLn("Flattened list: " ++ show (flatten nestedList))
