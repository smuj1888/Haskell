-- 1. The flatten :: [[a]]-> [a] function can be implemented as a list comprehension
--  flatten xss = [ x | xs <- xss, x <- xs ] allows us to flatten a nested list. Write flatten
--  using the list monad, both with do-notation and with explicit monadic notation.

import Control.Monad (join)
--flatten function using list comprehension
--takes a nested list of type [[a]] and flattens in into a single list [a]
flatten :: [[a]] -> [a]
-- List comprehension to extract each element from sublists
flatten xss = [x | xs <- xss, x <- xs]

--flatten fucnction using do notation
--Takes a nested list of type [[a]] and flattens it using do-notation
flattenDo :: [[a]] -> [a]
flattenDo xss = do
    xs <- xss
    x <- xs
    return x

--flatten function using do notation with join
flattenDoJoin :: [[a]] -> [a]
flattenDoJoin xss = join xss

--flatten function using explicit monadic notation
flattenMonad :: [[a]] -> [a]
flattenMonad xss = join (map return =<< xss)


--  2. Write a function superSafeDiv :: Int-> Int-> Maybe Int that divides the first number by the
--  second. The function should return Nothing if either you are dividing by zero, or if dividing would result
--  in a non-integer (e.g., superSafeDiv 4 2 would result in Just 2 whereas superSafeDiv 4 0 and
--  superSafeDiv 1 2 would result in Nothing.
 
superSafeDiv :: Int -> Int -> Maybe Int
superSafeDiv _ 0 = Nothing
superSafeDiv a b
    | a `mod` b == 0 = Just (a `div` b)
    | otherwise = Nothing

-- 3. Using the superSafeDiv function, write a function divTwice :: Int-> Int-> Maybe Int that
--  divides the first number by the second, twice. Again, return Nothing if you would be dividing by zero
--  or the first number is not evenly divisible by the second.
--  Write your function using both do-notation and monadic bind notation, without using explicit pattern
--  matching

-- The divTwice function uses do-notation to perform the division twice relying on superSafeDiv for safety
divTwice :: Int -> Int -> Maybe Int
divTwice a b = do
    --perform the first div
    firstDiv <- superSafeDiv a b
    --perform the second div
    superSafeDiv firstDiv b
-----
-- Both functions avoid explicit pattern matching and use the monadic nature of Maybe to handle failure cases, ensuring safety throughout
-----
--this function achieves the same as divTwice but uses monadic bind notation >>= to chain the divisions
divTwiceBind :: Int -> Int -> Maybe Int
--use a bind to chain the divisions
divTwiceBind a b = superSafeDiv a b >>= superSafeDiv b


