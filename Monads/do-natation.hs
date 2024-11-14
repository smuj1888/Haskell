--IO is a monad, we can also write IO computation using explicit >>= notation (without needing do blocks)

greetReverse :: IO ()
greetReverse = do
    name <- getLine
    let reverseName =
        reverse name
        putStrLn “hello”
        putStrLn reverseName

greetReverse :: IO ()
greetReverse =
    getLine >>= \name ->
    let reverseNme = reverse name in
    putStrLn "hello" >> putStrLn reverseName


--RULES OF DO NOTATION
--do notation is syntactic sugar (designed to make things easier to read or to express.)

--widely used and very usefull but sometimes quicker \ more concises to writ the monadic expression directly

-- do x <- M N              Monadic M >>= \x -> N 
-- do M N                   Monadic M >> N
-- do let x = M N           Monadic let x = M in N 
