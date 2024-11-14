--Evaluation strategies

-- Evaluation Strategies Overview:
-- The order of expression evaluation depends on both the language semantics and implementation

--Two main strategies
-- 1. Eager evaluation (Strict evaluation / call by value)
-- 2. Lazy evaluation (Non-strict evaluation / call by need)

-- Example of Lazy vs Eager Evaluation:

-- In Python, this would take a very long time to evaluate:
-- fib :: Int -> Int
-- A naive Fibonacci function is expensive to evaluate:
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- But in Haskell, due to lazy evaluation, this would terminate immediately:
useFirst :: (a -> c) -> a -> b -> c
useFirst f x _ = f x


-- Benefits of Lazy Evaluation:
-- 1. we can compute with large data structures ( including infinite lists)
-- 2. expensive functions are only evaluated when needed
-- 3. we can compute with dangerous values, such as undefined or bottom, which only cause problems if they are evaluated

-- Example of 'undefined' and 'bottom':
-- - `undefined` raises a runtime exception when evaluated.
-- - `bottom = bottom` is an infinite recursive type, causing infinite evaluation.
bottom :: a
bottom = bottom


--Have you encountered lazy evaluation before?
--in C operators && and ||
--in Python 3 range() function
--However, in general, most languages (imperative, OO and functional) operate with eager evaluation strategies; in that sense Haskell is unusual

-- Drawbacks of Lazy Evaluation:
-- 1. Difficult to combine with exception handling (Evaluation order in unclear)
--2. Can be harder to predict (lazy IO)
--3. causes runtime memory pressure as unevaluated expressions (thunks) build up

-- Interesting Infinite Lists:
-- Example of some infinite lists:
infiniteList1 = [1..]  -- Infinite list of non-negative integers
ones :: [Int]
ones = 1 : ones        -- Infinite list of 1s

-- Infinite list of primes using the Sieve of Eratosthenes:
primes :: [Int]
primes = sieve [2..]
  where sieve (x:xs) = x : sieve [x' | x' <- xs, x' `mod` x /= 0]

-- Infinite list of Fibonacci numbers:
fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- Useful Functions for Infinite Lists:
-- take :: Int -> [a] -> [a]
-- - Selects the first n elements from a list
takeExample = take 10 fibs  -- Take first 10 Fibonacci numbers

-- repeat :: a -> [a]
-- - Produces an infinite list of repeated values
repeatExample = take 5 (repeat 3)  -- [3, 3, 3, 3, 3]

-- cycle :: [a] -> [a]
-- - Produces an infinite list by appending a finite list to itself infinitely
cycleExample = take 6 (cycle [1, 2, 3])  -- [1, 2, 3, 1, 2, 3]

-- iterate :: (a -> a) -> a -> [a]
-- - Produces an infinite list of function applications to an initial value
iterateExample = take 5 (iterate (+1) 0)  -- [0, 1, 2, 3, 4]