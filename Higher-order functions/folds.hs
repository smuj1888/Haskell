
-- A fold is a way of reducing a list into a single value
-- We have a function which takes an element of the list, and an accumulator value, producing a new accumulator

-- There is 2 types of fold, depending on the desired associativity


--LEFT FOLD 
--Left fold is left-associative this means that the list is traversed in the order from left to right
-- brackets are placed to the left
--foldl :: (b -> a -> b) -> b -> [a] -> b


--RIGHT FOLD
--Right fold is right-associative this means that the list is traversed in the order from right to left
-- brackets are placed to the right
--foldr :: (a -> b -> b) -> b -> [a] -> b


--Example of a left fold
foldlCustom :: (accumulator -> element -> accumulator) -> accumulator -> [element] -> accumulator
--If the list is empty, return the initial value this is teh base case
foldlCustom _ initialValue [] = initialValue
--Next apply the function to the current element and the accumulator, then recusively process the remaining list elements
foldlCustom function initialValue (currentElement : remainingList) = foldlCustom function (function initialValue currentElement) remainingList


foldrCustom :: (element -> accumulator -> accumulator) -> accumulator -> [element] -> accumulator
--If the list is empty return the initial value this is the base case
foldrCustom _ initialValue [] = initialValue
--Next apply the function to the current element and the accumulator, then recusively process the remaining list elements
foldrCustom function initialValue (currentElement : remainingList) = function currentElement (foldrCustom function initialValue remainingList)

main :: IO ()
main = do
    
    let numbers = [1, 2, 3, 4, 5]
    
    -- test function for left fold that sums all elements in the list using foldlCustom
    --(+) takes the accumulator and the current element and returns the sum
    let sumLeft = foldlCustom (+) 0 numbers
    putStrLn $ "Sum using left fold " ++ show sumLeft

    -- \ is used to define an anonymous function that takes 2 arguments and returns the concatenation of the two
    let concatLeft = foldlCustom (\acc x -> acc ++ show x) "" numbers
    putStrLn $ "Concatenation using left fold " ++ concatLeft

    let sumRight = foldrCustom (+) 0 numbers
    putStrLn $ "Sum using right fold: " ++ show sumRight


    let concatRight = foldrCustom (\x acc -> acc ++ show x) "" numbers
    putStrLn $ "Concatenation using right fold - expected reverse order: " ++ concatRight





