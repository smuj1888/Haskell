
isbnCheck :: [Int] -> Int
isbnCheck digits 
    | length digits /= 9 = error "The input list must contain exactly 9 digits"
    | otherwise =  
        let sumDigits = sum digits
            remainder = sumDigits `mod` 11
        in if remainder == 0 then 0 else 11 - remainder

--helper function to calculate the weighted sum of the first 9 digits
sumDigits :: [Int] -> Int
sumDigits digits = sum [ (10 - i) * x | (i, x) <- zip [0..8] digits]


main :: IO ()

main = do
    let isbnDigits = [0, 2, 6, 2, 1, 6, 2, 0, 9]  -- Example ISBN without check digit
    let checkDigit = isbnCheck isbnDigits
    putStrLn ("The check digit for the given ISBN is: " ++ show checkDigit)