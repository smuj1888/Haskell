-- Sometimes we might want to have a computation that returns multiple pssible results : we can this a nondeterministic computation
--Example, A coin toss can eiher return heads or tails, how about if we wanted to build a list of the possible outcomes of two tosses?

data CoinToss = Heads | Tails deriving (Show, Eq)

coinToss :: [CoinToss]
coinToss = [Heads, Tails]

-- we want
--[(Heads, Heads), (Heads, Tails), (Tails, Heads), (Tails, Tails)]

--How is this implemented?
--We want to draw the first element from the first list, and use that result for the remaining computation
    --Then draw the first element of the second list, and return the pair
    --Then draw the second element of the second list and retunr the pair
    --and reapeat for the second element of the first list

--Coin tosses using withEach
withEach :: [a] -> (a -> [b]) -> [b]
withEach []     _ = []
withEach (x:xs) f = f x ++ (withEach xs f)

twoCoinTosses :: [(CoinToss, CoinToss)]
twoCoinTosses =
    coinToss `withEach` (\x ->
    coinToss `withEach` (\y ->    
    [(x,y)]))


--Coin tosses using List comprehension
twoCoinTossesList :: [(CoinToss, CoinToss)]
twoCoinTossesList = [ (x, y) | x <- coinToss, y <- coinToss ]
