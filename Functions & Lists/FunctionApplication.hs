-- Function that adds one to a number
addOne :: Int -> Int
addOne x = x + 1

main :: IO ()
main = do
    -- Call addOne with different expressions
    print (addOne 5)     -- Output: 6
    print (addOne 5 * 2) -- Output: 12 (equivalent to (addOne 5) * 2)
    print (addOne (5 * 2)) -- Output: 11 (equivalent to addOne 10)


