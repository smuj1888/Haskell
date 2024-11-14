import Control.Concurrent (yield)
--PARAMETRIC POLYMORPHISM
--Rotate
rotate :: (a,b,c) -> (c,a,b)
-- rotate the elements of a tuple to rearrange (c,a,b)
rotate (x,y,z) = (z,x,y)
------------------------------------------

--Currying
myCurry :: ((a, b) -> c) -> a -> b -> c
--converts a function that takes a tuple into a curried funtion
myCurry f x y = f (x, y)
------------------------------------------

--UnCurrying
myUnCurry :: (a->b->c) -> (a,b) -> c
--converts a curried function into a function that takes a tuple
myUnCurry f (x, y) = f x y
------------------------------------------


--FLIP
--flip the order of the arguments of a function
--takes a function that takes two arguments and returns a function that takes the arguments in reverse order
myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f y x = f x y
------------------------------------------

--SWAP
--Swaps the order of the first two arguments of the given function
--function definition for applying functions to tuple elements and combining the results
f :: (a, b) -> (a -> c) -> (b -> d) -> ((c, d) -> e) -> e
f(x,y) f1 f2 f3 = f3 (f1 x, f2 y)






main :: IO()
main = do
    --ROTATE
    --define a tuples with different types
    let originalTuple = (1, "hello", True)

    let rotatedTuple = rotate originalTuple
    print rotatedTuple

    --CURRYING
    --define a function that adds two numbers in a tuple
    let addPair(x, y) = x + y
    --apply myCurry to addPair
    let curriedAdd = myCurry addPair
    print (curriedAdd 3 4)

    --UNCURRYING
    --Apply myUnCurry to cconvert curriedAdd back in to a function that takes a tuple
    let unCurriedAdd = myUnCurry curriedAdd
    print (unCurriedAdd (3, 4))


    --FLIP
    --Define a function that subtracts two numbers
    let subtract x y = x - y
    --Apply myFlip to subtract
    let flipSubtract = myFlip subtract
    print (flipSubtract 10 6)


    --SWAP
    let funcA x = x * 2
    let funcB y = length y
    let combineFunc (a,b) = a + b

    let resultSwap = f (5, "abc") funcA funcB combineFunc
    print resultSwap