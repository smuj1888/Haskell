--We will motivate these (somewhat abstract) conepts with the now-familiar Maybe datatype, 
-- which is monadic. 
-- Consider a function allButLast :: [Char] -> Maybe [Char]. This function will return 
-- the first (n-1) characters of a string with (n) characters, wrapped up as a Just value. If the 
-- input string is empty, then the function evaluates to Nothing. 

-- removes the last character of a string, it returns Nothing if the input list is empty-
-- otherwise is returns Just with the truncated list
allButLast :: [Char] -> Maybe [Char]
--base case
allButLast [] = Nothing
--if there is only one character in the list, return an empty list wrapped in Just
allButLast [x] = Just []
--if the list has more than 1 character, remove the last character and return the rest of the list
allButLast (x:xs) = 
    --recursivly call allButLast on the rest of the list
    let rest = allButLast xs
    in case rest of
        --if the rest is nothing then return just with the current character
        Nothing -> Just [x]
        -- if the rest is Just, prepend the current character to the result
        -- if rest is nothing, it means that the rest of the list was empty
        (Just xs') -> Just (x:xs')

takeExample :: [Maybe [Char]]
takeExample = take 10 $ iterate (\x -> x >>= allButLast) (Just "abcdef")


--Same function but with Either instead of Maybe

allButLast' :: [Char] -> Either String [Char]
allButLast' [] = Left "Empty list"
allButLast' [x] = Right []
allButLast' (x:xs) = 
    let rest = allButLast' xs
    in case rest of
        Left s -> Right [x]
        Right xs' -> Right (x:xs')

takeExampleEither :: [Either String [Char]]
--can also use (>>= allButLast) instead of (\x -> x >>= allButLast')
takeExampleEither = take 10 $ iterate (\x -> x >>= allButLast') (Right "abcdef")