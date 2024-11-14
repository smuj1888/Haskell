--Quickcheck is a property-based testing library in haskell
--it allows us to specify properties about our functions ( rules that should always be true )
--and then automatically tests these properties with random inputs

--The key steps for using quickcheck are
-- 1. define a property
-- A property is a function that returns a boolean
-- it describes a behavior or rule that we expect our funtion to follow 

-- 2. Run quickcheck
-- quickcheck generates random inputs and check if the property holds true for all cases
-- if it finds an input where the property fails, it reports this input as a counterexample

-- 3. Shrinking counterexamples
-- if quickcheck finds a failing test case, it attempts to shrink the input to a simpler case
-- that still fails, making it easier to debug 



-- Example: 
-- A property that checks if the length of a list matches the number of elements:
-- prop_len :: Int -> Bool
-- prop_len n = if n >= 0 then length [1..n] == n else True
-- This property checks if the length of a list from 1 to n is equal to n, only for non-negative integers.

-- QuickCheck automatically generates random values for 'n' and tests the property.
-- Use quickCheck to run tests: quickCheck prop_len

-- Use verboseCheck to see the test inputs: verboseCheck prop_len

-- QuickCheck is useful for testing large numbers of cases and finding edge cases that
-- are difficult to think of manually, making it a powerful tool for functional programming.


import Test.QuickCheck

-- Property 1: A list containing 'n' elements should have length 'n'
prop_len :: Int -> Bool
prop_len n = if n >= 0 then length [1..n] == n else True

-- isAscending: Checks if a list of integers is in ascending order
isAscending :: [Int] -> Bool
isAscending [] = True
isAscending [x] = True
isAscending (x:y:xs) = (x < y) && isAscending (y:xs)

-- Property 2: Any sublist of the natural numbers starting at 1 should be ascending
prop_asc1 :: Int -> Bool
prop_asc1 n = isAscending (take n [1..])

-- Property 3: A list that contains repeated values is not ascending (except for length 1 or less)
prop_asc2 :: Int -> Bool
prop_asc2 n = if n < 2 then True else not (isAscending (take n (repeat 1)))

-- Property 4: Adding the length as an element at the end of an ascending list makes it not ascending
prop_asc3 :: Int -> Bool
prop_asc3 n = if n <= 0 then True else not (isAscending ([1..n] ++ [n]))

-- Main function to run the tests
main :: IO ()
main = do
  -- Test the length property
  putStrLn "Testing list length property (prop_len):"
  quickCheck prop_len
  
  -- Test the ascending properties
  putStrLn "\nTesting ascending property with natural number sublists (prop_asc1):"
  quickCheck prop_asc1
  
  putStrLn "\nTesting non-ascending property with repeated elements (prop_asc2):"
  quickCheck prop_asc2
  
  putStrLn "\nTesting list that becomes non-ascending by adding its length at the end (prop_asc3):"
  quickCheck prop_asc3
