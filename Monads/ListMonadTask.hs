import Control.Concurrent (dupChan)


generatePairs :: [a] -> [b] -> [(a,b)]
generatePairs xs ys = do
    --pulls each element x from the first list
    x <- xs
    --pulls each element y from the second list
    y <- ys
    --wraps each pair into a list which the list monad concatenates automatically
    return (x, y)

--This can also be written using bind
generatePairsBind :: Monad m => m a -> m b -> m (a, b)
generatePairsBind xs ys = xs >>= \x -> ys >>= \y -> return (x, y)


main :: IO ()
main = do
    print (generatePairs [1, 2] ['a', 'b'])
    print (generatePairsBind [1, 2] ['a', 'b'])

