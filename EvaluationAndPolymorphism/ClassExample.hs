data Mammal = Human | Dog | Cat | Cow deriving (Show,Eq)

data Reptile = Crocodile | Snake deriving (Show, Eq)

class Noisy a where
    mkNoise :: a -> String

instance Noisy Mammal where
    mkNoise Dog = "woof"
    mkNoise Cat = "meow" 
    mkNoise Cow = "moo" 
    mkNoise _ = "ow"

instance Noisy Reptile where
    mkNoise Crocodile = "snap"
    mkNoise Snake     = "hiss"
    
verse :: (Show a, Noisy a) => a -> String
verse animal = "Old MacDonald had a farm, EIEIO\n" ++
               "And on that farm he had some " ++ (show animal) ++ "s EIEIO\n" ++
               "With a " ++ noiseTwice ++ " here and a " ++ noiseTwice ++ " there\n" ++
               "Here a " ++ noiseOnce ++ ", there a "  ++ noiseOnce ++ "\n" ++
               "Everywhere a " ++ noiseTwice ++ "\n" ++
               "Old MacDonald had a farm, EIEIO\n\n"
                 where noiseOnce = (mkNoise animal)
                       noiseTwice = noiseOnce ++ "-" ++ noiseOnce


song :: (Show a, Noisy a) => [a] -> String
song animals = concatMap verse animals

main = do
    putStrLn $ song [Dog, Cat, Cow]
    -- putStrLn $ song [Dog, Cat, Cow, Crocodile, Snake]