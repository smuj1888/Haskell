--we can think of a using a monad as being like putting a value into a structure, elevating or lifting the value into a monad
--list is a monad, and we can use it to represent non-deterministic computations

-- List return simply puts a value into a singleton list ( i.e syntactically just put square brackets around it)
listReturn :: a -> [a]
listReturn x = [x]


--the bind takes each element out of the list applies a function to it, and then concatenates the results into a signel new results list 
listBind :: [a] -> (a -> [b]) -> [b]
listBind xs f = concat $ map f xs


-- ghci> let f x = [x,x]
-- ghci> f 2
-- [2,2] 
-- ghci> [1,2,3] >>= f
-- [1,1,2,2,3,3]
-- ghci>        

-- ghci> let f = \x -> (show x) ++ " mississippi... "
-- ghci> [1,2,3] >>= f
-- "1 mississippi... 2 mississippi... 3 mississippi... "

