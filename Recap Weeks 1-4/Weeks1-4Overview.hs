--EXPRESSIONS V STATEMENTS
--in an imperative language such as java a statement is an instruction
--computation driven by evaluatings statements to maniulate program counter / control flow

--An expression is a potentially complex term in the language that will eventually evaluate to a value (e.g 10)

--In a funtional lanugage like haskell, everything is an expression and this is the main thing that sets FP apart from imperative languages


--Reduction
--
-- Computation happens by reducing a expression towards a value
--Given that we dont have side effects, reduction satidies the church rosser property : No matter how we choose to reduce the expression, we will always get the same value
--This is not the case in imperative languages where the order of evaluation can change the result
--In haskell, we can reduce expressions in any order and still get the same result

--Example
--Is the following code legal in haskell???
if (if 5 < 10 then True else False) then
 “Branch 1”
else
 “Branch 2”

-- No: since if is a statement, it cannot be used inside another if
-- No: both if-then-else blocks must have the same type
Yes: this is OK since the inner if has type Bool 


--FUNCTIONS AND CURRYING
-- functions take parameters and produce a result
--Haskell functions are FIRST CLASS and can be let-bound, passed around as arguments, and returned as results
--Functions can be anonymous (lambda functions) or named using an equation

--CURRYING
--Strictly speaking, a function takes only one parameter
--We can get multi argument functions in two ways :
--1. Using tuples for all arguments
--2. Currying, where a function return another function

--Currying has the advanage of allowing partial application, where we can specialize a function to a given argument

--Example
Assume a function ordinal :: Int -> String, where 
ordinal 1 = “first”
ordinal 2 = “second”, etc.
• What is the type of the function:
greetMonarch title firstname num =
 “Hello, “ ++ title ++ “ “ ++ firstname ++ 
 “ the ” ++ (ordinal num)

--greetMonarch :: String -> Int -> String
greetMonarch :: String -> String -> Int -> String 
--greetMonarch :: a -> a -> Int -> a
--greetMonarch :: String -> String -> String -> String

--GreetMonarch takes 3 arguments, the first two are strings and the last is an int and returns a string

--LIST AND LIST COMPREHENSIONS
--A list is an ordered sequence of elements of the same type
--we can use sequence notation to create ordered lists, these can potentially be infinite
--we can also create lists using list comprehensions

--Deconstructing lists
--we can deconstruct lists using accessor functions, these are a bit of code smell, as they may fail
--we can also use pattern matching to deconstruct lists, this is the preferred way
(x:xs) - x is the head of the list, xs is the tail


--Algebraic data types, pattern matching and recursion

--Algebraic data types
--Algebraic data types : different ways of constructing data types (Hearts :: Suit, Diamonds :: Suit, etc.)
--when writing a fucntion on an ADT, need to pattern match
data Colour = Red | Black

data Suit =
    Hearts | Diamonds 
    | Clubs | Spades

data Card = 
    King Suit
    | Queen Suit
    | Jack Suit
    | Ace Suit
    | Number Suit Int

getColour :: Suit -> Colour
getColour Hearts = Red
getColour Diamonds = Red
getColour Clubs = Black
getColour Spades = Black


--Writing recursive functions
--Think about the structure you are defining recursion on : Integers? Lists? Trees?
--Write a base case : you will want at least pure functions to terminate
listSum [] = 0 <<< This is the base case incase the list is empty

-- write a recursive or inductive case which calls the same function with an argument which converges to the base case
listSum (x:xs) =
 x + (listSum xs)

--this recursive case adds current element to the sum of the rest of the list (xs)


--Higher order functions
--A higher order function is a function which takes another function as an argument
--example
-- this function takes a function f and a list xs and applies f to each element of the list
map :: (a -> b) -> [a] -> [b]
--base case
map f [] = []
--recursive case, apply f to the head of the list and then map f to the tail of the list
map f (x:xs) = (f x) : (map f xs)

--it is good practive to use HOFs where applicable rather than hand roll recursive functions yourself
--concetrate on the logic of the function you are writing rather than the mechanics of the recursion
--often conveniant to supply a HOF with an anonmyous lambda function

--COMMONG HIGHER ORDER FUNCTIONS
--map : apply a function to each element of a list
map :: (a -> b) -> [a] -> [b]
--filter : keep only elements of a list that satisfy a predicate
filter :: (a -> Bool) -> [a] -> [a]
--twice : apply a function twice
twice :: (a -> a) -> a -> a

--FOLDS
--A fold is a HOF, and a way of reducing a list into a single value
--foldl and foldr are the two most common folds
--foldl is left associative, foldr is right associative
--foldl is tail recursive, foldr is not
--foldl is more efficient than foldr

foldl (+) 0 [1, 2, 3, 4]

(((0 + 1) + 2) + 3) + 4

10

foldr (+) 0 [1, 2, 3, 4]

(1 + (2 + (3 + (4 +0)))

10


--Property based testing / quickcheck
-- we often want to see whether a function satisfies a property
-- property based testing ( as implemented by quickcheck) will generate many random instances and check whether a property holds
--if not it will provide a counterexample

-- check that a list [1..n] has length n
prop_len :: Int -> Bool
prop_len n = 
    if n >= 0 then
        length [1..n] == n
    else True

--Evaluation strategies
--Haskell uses lazy evaluation : this means that computations only happen as they are needed
--This is in contract to eager evaluation, where computations are evaluated earlier ( e.g. when passed as arguments to a function)
--we cn use this to, for example create infinite lists, or to avoid evaluating a computation that is not needed

--example question 
• Suppose we have a function blowup x = blowup (x + 1).
What would be the result of evaluating const 5 (blowup 1)?
                        ^^
• The program would immediately evaluate to return 5 

--Polymorphism
--parametric polymorphism : a function is polymorphic if it can be applied to arguments of different types
-- some fucntions only use their arguments structurally so applying a supplied function, or reversing a list
--therefor they can have many different types
--we can therefore give them very general types using type variables
--the types sometimes mean there is only one implementation due to parametricity

--ad-hoc polymorphism via typeclasses
--java supports ad-hoc polymorphism using function overloading for example we could efine an open fucntion on a socket, or stdout or a file
--haskell supports ad-hoc polymorphism using typclasses : a specification of the functions that an instance must implement




