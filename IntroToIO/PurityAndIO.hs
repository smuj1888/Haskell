--Purity
--1. Stateless : The function always evaluates to the same result, given the same arguments.
--2. No Side Effects : the function does nothing apart from simple expression evaluation - no interaction with the 'outside world’
--3. Total : The function is defined for all possible inputs.

--Whats the problem with Purity?
-- Out code needs to do I/O
 -- to get input from peripherals
    -- to display output to the user
    -- to read and write files
    -- to communicate over a network

--But these are externally visible operations - side effecting code

--Naive approach
--1. We lose church-rosser property
--2. We lose referential transparency ( the property that we can replace a piece of code with the value it produces)
--3. It wreacks havoc with lazy evaluation
--4. and therefore we lose all our reasoning

--IO TYPES
-- Each side effecting operating is marked with a type constructor IO
-- () Unit type contains no information
-- IO Computation that will produce the unit value
-- Function that takes a string, and returns a computation that will produce the unit value
putStrLn :: String -> IO ()


--MIXING PURE FUNCTIONS AND IO COMPONENTS
--Dont do reverse getLine
--Instead do 
reverse :: String -> String
getLine :: IO String

--A string is not the same an IO String
--A string is data, an IO string is a computation which produces a string

--Side-effecting computations are always marked in their types.


--MIXING PURE FUNCTIONS AND IO COMPUTATIONS
-- do marks that were putting together a computation
--within a do block the <- operator allows us to give a name (str) to the result of an IO operatiion here str has type String
--let revStr = reverse str Binds the result of pure reverse function to revStr using let
-- Print reversed string to the console 
getAndPrintReverse :: IO ()
 getAndPrintReverse = do
 str <- getLine
 let revStr = reverse str
 putStrLn revStr


--IO COMPUTATINOS THAT RETURN VALUES
-- IO String is defining an IO computation that will return a String
--  Note that we have to use the return functionreturn :: a -> IO a  -- roughly(since revStr is of type String, and we need an IO String)
 getAndReverse :: IO String
 getAndReverse = do
 str <- getLine
 let revStr = reverse str
 return revStr


--THE BIGGER PICTURE
--Every Haskell program has an entry point, main
 main :: IO ()
 main = do
 line <- getLine
 putStrLn (makeUpper line)

--Main function is evaluated when program is ran, it can then make use of other functions with IO type
--GCHI runs everything an an IO computation, which is why we can print things out
-- Good practice : keep as much of the program as pure as possible, Pure functions are much easier to test and reason about

--ESCAPING IO
--There is no way to project a value out of an IO computation
--While you might want a function with type IO String -> String, this doesnt make sense : we would run into the same issues with Chruch rosser as before
--Instead thing of IO as thought you are using do-notation to build a bigger computation by stringing together smaller IO computations, with main as your entry point

--TRACE DEBUGGING
--It is someitimes useful to do print debugging, where we wish to print some program state to the console
-- we can do this using the TRACE function
import Debug.Trace
trace :: String -> a -> a
--This uses unsafePerformIO under the hood, and should only be used for debugging

--BEYOND CONSOLE IO
--IO emcompasses a large number of impure operations beyond just reading/ writing to the console
--getting current time
getCurrentTime :: IO UTCTime

--FILE OPERATIONS
readFile :: FilePath -> IO String
writeFile :: FilePath -> String -> IO ()

--Also sockets, graphics, printing, spawning processes

--PSUEDO RANDOM NUMBER GENERATION
--random number generaion might seem to be an impure function
--In fact a pseudo random number generator generates a random value, and a new generator
-- only seeding the PRNG is impure, generation is pure

--REFERENCE CELLS
newIORef :: a -> IO (IORef a) --- creates a new IO reference with an initial value of type a
readIORef :: IORef a -> IO a --- reads the value of an IO reference
writeIORef :: IORef a -> a -> IO () --- writes a new value to an IO reference


