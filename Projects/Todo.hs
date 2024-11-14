
--main entry to the program

main :: IO ()
main = do
    putStrLn "-------Todo List Application-------"

    --call the mainMenu function
    mainMenu []


--addTodo function and binding
addTodo :: [String] -> IO ()
addTodo todos = do
    --get the userinput
    putStrLn "Enter your new todo"
    newTodo <- getLine
    --append the new todo to the current todo list
    let updatedTodos = todos ++ [newTodo]
    putStrLn ("Added " ++ newTodo)
    --return to the mainMenu with the new updated list 
    mainMenu updatedTodos


--Viewing todos function and binding
viewTodos :: [String] -> IO ()
--check if the list is empty if so back to the main menu
viewTodo [] = do
    putStrLn "Your todo list is empty"
    mainMenu []

--mapM_ is a variant of map that is used for functions with side effects (like putStrLn which prints to the console). 
--The underscore (_) means that we are discarding the result (i.e., not returning a list of printed lines).
--mapM_ putStrLn applies putStrLn to each element of the list and prints each item in sequence.

--[1..] is an infinite list of numbers and are used in this case to generate the indices for the map
--The anonymous function (\n todo -> show n ++ ". " ++ todo) combines each index (n) with its corresponding todo item (todo).
viewTodos todos = do
    putStrLn "Your todo list : "
    mapM_ putStrLn (zipWith (\n todo -> show n ++ ". " ++ todo) [1..] todos)
    mainMenu todos


--marking a todo as done
markDone :: [String] -> IO ()
markDone todos = do
    putStrLn "Enter the number of the todo you completed: "
    numberStr <- getLine
    let number = read numberStr :: Int
    if number > 0 && number <= length todos
    then do
        --here i split the list into two parts using splitAt, remove the completed item and then concatenate the 2 parts of the list back together
        let (before, _:after) = splitAt (number - 1) todos
        let updatedTodos = before ++ after
        putStrLn "Todo marked as done!"
        mainMenu updatedTodos
    else do
        putStrLn "Invalid number"
        mainMenu todos 


--deleting a todo
deleteTodo :: [String] -> IO ()
deleteTodo todos = do
    putStrLn "Enter the number of the todo you want to delete:"
    numberStr <- getLine
    let number = read numberStr :: Int 
    if number > 0 && number < length todos
    then do
        --here i split the list into two parts using splitAt, remove the completed item and then concatenate the 2 parts of the list back together
        let (before, _:after) = splitAt (number - 1) todos
        let updatedTodos = before ++ after
        putStrLn " Todo deleted"
        mainMenu updatedTodos
    else do
        putStrLn "Invalid number"
        mainMenu todos



--mainMenu function
--this function takes a list of strings as an argument
mainMenu :: [String] -> IO ()
mainMenu todos = do
    putStrLn "Pick an option:"
    putStrLn "1. Add a todo"
    putStrLn "2. Display todos"
    putStrLn "3. Mark todo as done"
    putStrLn "4. Remove todo"
    putStrLn "5. Exit"
    --get the user input for the selection they want
    option <- getLine
    handleOption option todos

--handleOption function
--this function takes the user input and the list of todos as arguments
handleOption :: String -> [String] -> IO ()
handleOption option todos = 
    case option of
        "1" -> addTodo todos
        "2" -> viewTodos todos
        "3" -> markDone todos
        "4" -> deleteTodo todos
        "5" -> putStrLn "Exiting..."
        _ -> do
            putStrLn "Invalid option, please try again"
            mainMenu todos