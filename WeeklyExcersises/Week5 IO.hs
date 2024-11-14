--  1. Write a function, echoCaps :: IO (), which reads in a line of text, capitalises it, and then prints it
--  to the console. (Hint: If you write import Data.Char, you can use the toUpper :: Char-> Char
--  function along with a list comprehension to capitalise a string)
import Control.Monad (forever)
import Data.Char (toUpper)
echoCaps :: IO ()
echoCaps = do
    putStrLn "Enter a line of text:"
    --read input
    input <- getLine
    -- capitalise input using a list comprehension
    let caps = [toUpper c | c <- input]
    --print capitalised input
    putStrLn caps


--  2. Using the following functions (noting that FilePath is a type synonym for String):
--  • readFile :: FilePath-> IO String (which reads a file with the given path)
--  • lines :: String-> [String] (which separates a string into a list of strings)
--  Write a function echoFile :: FilePath-> IO () which prints out a file to the console, line-by-line

echoFile :: FilePath -> IO ()
echoFile filePath = do
    --read the content of the file
    contents <- readFile filePath
    --splt the content into lines
    let allLines = lines contents
    --print each line mapM_ is used to print each line
    mapM_ putStrLn allLines


--  3. Using the getLine :: IO String, read :: String-> a, and show :: a-> String functions,
--  write a function calculator :: IO () which:
--  • Gets an operation (one of +,−,∗) from the command line; if the input does not match one of
--  these then default to +
--  • Gets two numbers using two calls to getLine
--  • Outputs the operation applied to the two integers

calculator :: IO ()
calculator = do
    putStrLn "Enter an operation (+, -, *):"
    --get user input
    operation <- getLine
    --get two numbers
    putStrLn "Enter first number:"
    num1 <- getLine
    putStrLn "Enter second number:"
    num2 <- getLine
    --apply the operation to the two numbers
    let result = case operation of
            "+" -> (read num1 :: Int) + (read num2 :: Int)
            "-" -> (read num1 :: Int) - (read num2 :: Int)
            "*" -> (read num1 :: Int) * (read num2 :: Int)
            _ -> (read num1 :: Int) + (read num2 :: Int)
    --print the result
    print result



--  4. Using the following functions:
--  • forever :: IO a-> IO b(whichperformsanIOactionforever;youwillneedtoimportControl.Monad)
--  • getLine :: IO String (which reads a line from the console)
--  • appendFile :: FilePath-> String-> IO () (which appends a string to a given file)
--  Write a function infiniteAppend :: IO () which:
--  (a) Gets a file path from the console
--  (b) Gets a line from the console, and appends it to the file
--  (c) Repeats step (b)


infiniteAppend :: IO ()
infiniteAppend = do
    --get the file path from the console
    putStrLn "Enter the file path:"
    filePath <- getLine
    --repeatedly get a line of input and append it to the file
    forever $ do
        -- get a line of input
        putStrLn "Enter a line to append to the file:"
        line <- getLine
        --append the line to the file
        appendFile filePath (line ++ "\n")



-- Main function to test all other functions
main :: IO ()
main = do
    putStrLn "Choose a function to test:"
    putStrLn "1. echoCaps"
    putStrLn "2. echoFile"
    putStrLn "3. calculator"
    putStrLn "4. infiniteAppend"
    choice <- getLine
    case choice of
        "1" -> echoCaps
        "2" -> do
            putStrLn "Enter the file path:"
            filePath <- getLine
            echoFile filePath
        "3" -> calculator
        "4" -> infiniteAppend
        _   -> putStrLn "Invalid choice. Please enter a number between 1 and 4."
