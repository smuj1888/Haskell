
--Potentially failing computations
--Maybe type
-- data Maybe a = Just a | Nothing

--The maybe a data type is like a type safe null, and is used to define potentially failing computations 
--EXAMPLES 
-- both Just 5 and nothing can have type Maybe Int, altought only Just 5 actually contains an int

--Whenever we want to use a value of type Maybe a, we will need to case split to see whether it contains a value or not

--Managing multiple Maybes
--The maybe type allows us to return Nothing if a computation fails

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (a `div` b)

-- Suppose we want to use the safeDiv function twice, and add the results.

divAndAdd :: Int -> Int -> Int -> Int -> Maybe Int
divAndAdd x y d1 d2 =
  case (safeDiv x d1, safeDiv y d2) of
    (Just x', Just y') -> Just (x' + y')
    (_, _)             -> Nothing

-- divAndAdd : Divide numbers x by d1, and y by d2, and add the results together.Applicative
-- We are having to write a lot of boilerplate to check whether a value is present or not
--Worse, this style may lead to deeply-nested case statements, making code very difficult to read, write, and maintain
--Is there a way that we can write divAndAdd as if each safeDiv computation succeeds, and have it automatically evaluate to Nothing if any subcomputation fails?

--Better way of doing it
--We can use higher-order functions to abstract away the boilerplate code

--A function that takes a Maybe a, and a function that we can pass the unwrapped value to if it exists? if it doesnt we return Nothing
--Maybe a is the potentially failing computation
--(a -> Maybe b) the function to run with the Just value, if it exists 
--Maybe b is the result of the computation
withJust :: Maybe a -> (a -> Maybe b) -> Maybe b
withJust Nothing _ = Nothing
withJust (Just x) f = f x

--Using our withJust function 
--safeDiv 10 5 The potentially failing computation
--`withJust` our withJust function, that tries to unwrap but returns nothing otherwise
-- \x in the continuation function, x has type Int as weve already unwrapped it in withJust


-- Main function to test withJust
main :: IO ()
main = do
    let result = safeDiv 10 5 `withJust` (\x -> 
                 safeDiv 20 10 `withJust` (\y -> 
                 Just (x + y)))
    case result of
        Nothing   -> putStrLn "Computation failed."
        Just sum  -> putStrLn $ "The result is: " ++ show sum