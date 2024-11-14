

greetUser :: IO ()
greetUser = do
    putStrLn "What is your name? "
    name <- getLine
    putStrLn "How old are you: "
    age <- getLine
    putStrLn ("Hello, " ++ name ++ "! You are " ++ age ++ " years old.")


main :: IO ()
main = greetUser