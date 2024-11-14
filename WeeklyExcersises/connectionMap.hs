import Data.List
import Data.Ord
--  1. Let us define a connected word as a word which we can write using only letters which are the same or
--  adjacent on a QWERTY keyboard. For example, “dessert” and “pool” are connected words (e.g., ‘p’ is
--  adjacent to ‘o’, ‘o’ is the same letter, and ‘o’ is adjacent to ‘l’), but “trench” is not (as ‘n’ is not adjacent
--  to ‘e’).
--  Using the provided connectionMap definition which maps each character to all adjacent characters,
--  and the included words file, write:
--  (a) A function isConnected :: String-> Bool which returns True if a word is connected.
--  (b) A function connectedWords :: String-> IO [String] which takes the name of a file con
-- taining a list of words, and returns a list of connected words.
--  (c) A function printStats :: IO () which prints the longest 5 connected words, and the number
--  of connected words.
--  Note: you can use the sortBy :: (a-> a-> Ordering)-> [a]-> [a] function to sort the
--  list by length.
--  Seehttps://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Ord.html#t:Ordering
--  for more details on the Ordering data type.


-- Corrected connection map representing adjacent keys on a QWERTY keyboard
connectionMap :: [(Char, [Char])]
connectionMap = [
    ('q', "was"), ('w', "qasde"), ('e', "wsdr"), ('r', "edft"), ('t', "rfgy"), ('y', "tghu"),
    ('u', "yhji"), ('i', "ujko"), ('o', "iklp"), ('p', "ol"),
    ('a', "qws"), ('s', "qwedx"), ('d', "wersfcx"), ('f', "ertdgcv"), ('g', "rtyfhvb"),
    ('h', "tyugjbn"), ('j', "yuihknm"), ('k', "uiojlm"), ('l', "iopk"),
    ('z', "asx"), ('x', "zasdc"), ('c', "xsdfv"), ('v', "cfbg"), ('b', "vgfh"),
    ('n', "bhgj"), ('m', "nhjk")
    ]

-- Function to check if a word is connected based on the QWERTY connection map
isConnected :: String -> Bool
isConnected [] = True  -- An empty word is trivially connected
isConnected [x] = True -- A single character is always connected
isConnected (x:y:xs)
    | y `elem` getConnections x = isConnected (y:xs)  -- Check if next character is adjacent
    | otherwise = False                               -- Return False if the characters are not connected
  where
    getConnections c = case lookup c connectionMap of
                         Just adj -> c : adj  -- Include the character itself and its adjacents
                         Nothing -> [c]       -- If character is not found, consider only itself

-- Function to get the list of connected words from a file
connectedWords :: String -> IO [String]
connectedWords filePath = do
    content <- readFile filePath  -- Read file content
    let wordsList = lines content  -- Split content into individual words
    return (filter isConnected wordsList)  -- Filter and return connected words

-- Function to print statistics about the connected words
printStats :: IO ()
printStats = do
    connected <- connectedWords "words.txt"  -- Get the connected words from file
    let sortedWords = sortBy (comparing length) connected  -- Sort words by length
    let longestFive = take 5 (reverse sortedWords)         -- Take the longest 5 words
    putStrLn "Longest 5 connected words:"
    mapM_ putStrLn longestFive                            -- Print the longest words
    putStrLn $ "Number of connected words: " ++ show (length connected)  -- Print total number of connected words

-- Explanation:
-- The `isConnected` function checks if all characters in a word are either the same or adjacent on a QWERTY keyboard.
-- It uses a predefined `connectionMap` to look up the adjacent keys for each character.
-- The `connectedWords` function reads words from a file and filters out only the connected ones.
-- The `printStats` function prints the longest 5 connected words and the total count, making use of sorting by word length.
