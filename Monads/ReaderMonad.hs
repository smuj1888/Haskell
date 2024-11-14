--The reader monad is also know as the environment monad, its useful for reading fixed values from a shared state environment
-- and for passing this shared state between multiple function calls in a sequence


-- theRunning the Reader: runReader :: Reader r a -> r -> a takes a Reader computation and an environment, 
-- and produces the final result. Here, runReader conversation "martin" runs the conversation with the name "martin".

-- The Reader Monad is particularly useful for scenarios where multiple functions need access to some shared configuration or state, 
-- and you want to avoid explicitly passing this state around in every function call.


import Control.Monad.Reader

hi :: Reader String String
hi = do
    --get the environemnt value ( which is the name)
    name <- ask
    return ("Hello, " ++ name ++ "!")

bye ::Reader String String
bye = do
    ----get the environemnt value ( which is the name)
    name <- ask
    return ("Goodbye, " ++ name ++ "!")

--a converstation that uses both hi and bye
conversation :: Reader String String
conversation = do
    start <- hi
    end <- bye
    return (start ++ " ... " ++ end)

main :: IO ()
main = do
    let result = runReader conversation "martin"  -- Run the Reader monad with the environment set to "martin"
    putStrLn result  -- Output: "hello martin ... goodbye martin"