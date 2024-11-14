--The writer monad is particularly useful for scenarios where you want to accumulate some extra information (often logs) alongside your computations
--tell function : tel is used to add messages to the log,
-- in this example were logging every action we take

--runWriter function : runWritter :: Writer w a -> (a, w) : extracts both the final result a and the accumulated log w
-- here we use it to get the result of the computation and log messages

--chaining actions : addOne and double are combined in the compute using the do-notation
-- each action logs its operation and the log grows as the computation proceeds

import Control.Monad.Writer

addOne :: Int -> Writer String Int
addOne x = do
    tell ("Incremented " ++ show x ++ ", ")
    return (x + 1)

double :: Int -> Writer String Int
double x = do
    tell ("Doubled " ++ show x ++ ", ")
    return (x * 2)

compute :: Writer String Int
compute = do
    --initial log statement
    tell "starting with 0, "
    v1 <- addOne 0
    v2 <- double v1
    v3 <- double v2
    addOne v3

main :: IO ()
main = do
    let (result, log) = runWriter compute  -- Run the Writer monad and get both result and log
    putStrLn ("Result: " ++ show result)  -- Output: the final result
    putStrLn ("Log: " ++ log)  -- Output: the accumulated log