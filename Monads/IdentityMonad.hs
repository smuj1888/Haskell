--The identity Monad is one of the simplest and least abstract Monads in haskell
--It represents computations that simple return a value without any additional computational context
--like side effects, errors or state, essentioally its just a wrapper for values, and using it helps unify code that works with monads

import Control.Monad.Identity

doubleAndDecrement :: Int -> Identity Int
doubleAndDecrement x = do
    y <- Identity (x * 2)
    z <- Identity (y + 1)
    return z

main :: IO ()
main = do
    let result = runIdentity (doubleAndDecrement 5)
    print result