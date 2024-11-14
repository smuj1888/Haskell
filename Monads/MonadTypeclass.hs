--Monad typeclass has two characteristic methods :
    -- 1. return 
    -- 2. >>= (bind)
-- The return function puts something into the monad i.e boxes it upp in the structure
-- The bind function >>= injects a monad value into arbitary function f as a parameter 


-- The Monad typeclass defintion
class Applicative m => Monad m where
    --The bind function applies a monadic value to a function then returns a monadic value
    (>>=) :: m a -> (a -> m b) -> m b
    --The return wraps a pure value into a monadic context
    return :: a -> m a
    
    --optional fail method
    fail :: String -> m a
    fail = error

