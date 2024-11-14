-- A tuple is an ordered sequence of expressions with a known length. Tuples can contain values of different types.

-- tuple1 = (1, 2, "hello")
-- tuple2 = (1.0, 3)

-- Tuples can be deconstructed or split up into their individual parts using fst and snd for first and second elements from a 2-element tuple or pair.

-- Define the addPair function
addPair :: (Int, Int) -> Int
-- uncurry (+) takes the curried addition function + which normally takes two arguments like x + y 
-- and tranforms it into a fucntion that takes a tuple
-- instead of calling + with two seperate arguments, we can call the new uncurry (+) function with a tuple containing two values like (10, 20)
addPair = uncurry (+)

-- Main function to test addPair
main :: IO ()
main = do
    -- Call the addPair function
    let result = addPair (10, 20)

    -- Print the result
    print result
