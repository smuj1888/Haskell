-- define a min function using if then else
minValue :: Int -> Int -> Int
minValue x y = if x < y then x else y


main :: IO ()
main = do
    let resultMin = minValue 10 5
    print resultMin