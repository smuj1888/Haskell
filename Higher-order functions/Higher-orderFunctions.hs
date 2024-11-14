import Data.Char (toUpper)

--Example of a higher order function is a map
-- This map function takes a function and a list and applies the function to every element in the list
applyFunctionToList ::(a -> b) -> [a] -> [b]
--if the list is empty, return an empty list
applyFunctionToList applyToElement [] = []
-- apply the function to the current element, and recursively call applyFunctionToList on the remaining elements.
applyFunctionToList applyToElement (element:remainingElements) = 
    (applyToElement element) : (applyFunctionToList applyToElement remainingElements)

-- A map applies a function to ever element in a list
--defined recursively by building up a new list with a function applied to each element


-- filter retains every element which satisfies a condition 
--only prepend the element if the condition evaluates to TRUE

-- the filterlist function takes a condition (a function that returns a boolean) and a list
-- it returns a new list with only the elements that satisfy the condition
filterList :: (a -> Bool) -> [a] -> [a]
-- base case if the list is empty return an empty list
filterList condition [] = []
filterList condition 
    -- check if the current element satisfies the condition
    (element : remainingElements) = if (condition element) 
    -- if the condition is true include the lement in the result and recursively call filterList on the remaining elements
    then (element : filterList condition remainingElements)
    -- if the confition is false then skip the element and continue filtering the remaining elements
    else filterList condition remainingElements



main :: IO ()
main = do
    --list of numbers to use for testing 
    let numbers = [1,2,3,4,5]
    --list of words to use for testing
    let wordsList = ["hello", "world", "Martin", "is"]

    -- example usage of applyFunctionToList
    --applies the function (*2) to every element in the list
    let doubledNumbers = applyFunctionToList (*2) numbers
    putStrLn " Doubled Numbers: "
    print doubledNumbers

    -- example usage of applyFunctionToList
    -- applies the function toUpper to every element in the list
    let uppercaseWords = applyFunctionToList (map toUpper) wordsList
    putStrLn "Uppercase Words:"
    print uppercaseWords


    -- example usage of filterList
    let evenNumbers = filterList even numbers
    putStrLn "Even Numbers:"
    print evenNumbers

    let longWords = filterList (\word -> length word > 4) wordsList
    putStrLn "Long Words:"
    print longWords