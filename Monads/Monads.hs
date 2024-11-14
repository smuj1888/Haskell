import Data.Bits (Bits(xor))
--MONADS
withJust :: Maybe a -> (a -> Maybe b) -> Maybe b

withEach     :: [a]  -> (a -> [b]) -> [b]

withIOResult :: IO a -> (a -> IO b) -> IO b

--In each case we have :
--1. A computation producing a result ( be it potentially failing, nondeterministic or side effecting)
--2. A function that builds a bigger computatio from the result
--3. The overall result being the bigger computation

--Maybe, list and IO are all instances of a more general pattern known as a Monad!
--To be a monad a data type needs two things:
    --A way of constructing a trivial computation
        -- a -> Maybe a (we can use Just)
        -- a -> [a] (we can use the singleton list constructor)
        -- a -> IO a (its a library function, but it creates a pure computation without side effects)
    --And a way of building a lrager computation from the results of a previous one

--THE MONAD TYPECLASS

-- Applicative m (Every monad must also be an applicative functor)
class (Applicative m) => Monad m where
    -- return injects a pure value into the monad, for example return 5 = Just 5 in the Maybe monad
    return :: a -> m a
    -- >>= (bind) allows us to build up a computation, takes a computation of type m a, and a function a -> m b to build a new computation m b and return m b
    (>>=) :: m a -> (a -> m b) -> m b
    -- >> (sequence) runs but ignores first computation, returns results of second derivable from definition of (>>=)
    (>>) :: m a -> m b -> m b


--WHAT IS A MONAD
-- A monad is just a data type that implements the Monad typeclass, and satisfies the Monad laws 


--EXAMPLE THE MAYBE MONAD
-- Can sequence many computations, but if any return Nothing then the whole computation returns nothing
-- we are just implementing >>= the same as our withJust function

instance Monad Maybe where
    return x = Just x
    (Just x) >>= f = f x
    Nothing >>= f = Nothing


--MANAGING MULTIPLE MAYBES MONADICALLY
-- here we use the bind operator >>= and provide a function where we assume each safe division has succeeded
-- we can then use return to create a Maybe Int from res1 + res2 but if any sub computation fails, then the whole result will be nothing

divAndAdd' :: Int -> Int -> Int -> Maybe Int
divAndAdd' x y d1 d2 =
    safeDiv x d1 >>= (\res1 -> 
    safeDiv y d2 >>- (\res2 ->
    return (res1 + res2)))