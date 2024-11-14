-- parametric polymorphism works for any type "a"
--It returns the value that was passed in
identity :: a -> a
identity x = x

-- function that reverses a list, it works for lists of any type because of  the type variable "a"
reverseList :: [a] -> [a]
-- an empty list is already reversed
reverseList [] = []
-- this is a recursive case reverse the rest, then append the first element
reverseList (x:xs) = reverseList xs ++ [x] 

main :: IO ()
main = do
    -- use different types to test it
    putStrLn ("Identity of 5: " ++ show (identity 5))
    putStrLn $ "Identity of 'Hello': " ++ identity "Hello"

    -- using the reverse List function on a list of int;s 
    putStrLn $ "Reversed list: " ++ show (reverseList [1, 2, 3, 4]) 