--The state monad is is similar to the reader and writer monads,
-- but it allows us not only to read from a shared state but also to update it
--this is useful when you want to maintain and manipulate some state throughout a sequence of operations

import Control.Monad.State

--fibonacci function using the state monad
fibState :: State (Integer, Integer, Integer) Integer
fibState = do
    --get the current state, which is a tuple (x1, x2, n)
    (x1, x2, n) <- get
    if n == 0 
        then return x1 -- if n is 0 then return the first value
        else do
            --update the state to and shift (x1, x2) and decrement n
            put (x2, x1 + x2, n - 1)
            --recursively call fibState to continue the sequence
            fibState

main :: IO ()
main = do
    let (result, finalState) = runState fibState (0, 1, 10)  -- Run the State monad with initial state (0, 1, 100)
    print result  -- Output the 100th Fibonacci number
    print finalState  -- Output the final state


-- ghci> runState fibState (0, 1, 5)
-- (5,(5,8,0))

