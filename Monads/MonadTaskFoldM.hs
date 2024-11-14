import Control.Monad (foldM)
import Text.ParserCombinators.ReadP (satisfy)

-- function takes a tuple (x,y) and returns Maybe Int and the function will check if y (the divisor) is 0
safeDiv :: (Int, Int) -> Maybe Int
safeDiv (x,y) =
    if y == 0
        then Nothing
        else Just (x `div` y)

--function that uses safeDiv on a list of pairs and chains the results using >>=
safeDivChain :: [(Int, Int)] -> Maybe Int
--foldM is the Monadic way of using foldL
-- foldM goes through each pair in the list, applying safeDiv and updating the accumulated result if Just
-- if safeDiv ever returns Nothing then the entire chain stops
safeDivChain pairs = foldM (\acc pair -> safeDiv (acc, snd pair)) (fst (head pairs)) pairs

main :: IO ()
main = do
    -- Output: Nothing
    print (safeDivChain [(10, 2), (15, 3), (10, 0)])
    -- Output: Just (some result) 
    print (safeDivChain [(10, 2), (5, 1), (1, 1)])         
