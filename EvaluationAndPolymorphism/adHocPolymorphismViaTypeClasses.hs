-- Functional Programming: Ad-hoc Polymorphism via Typeclasses

-- Ad-hoc Polymorphism Overview:
-- - Ad-hoc polymorphism is similar to method or operator overloading in languages like Java.
-- - Example in Java:
--   public float add(float f1, float f2) { … }
--   public int add(int i1, int i2) { … }
-- - It’s called ad-hoc because it’s not a core part of the type system like parametric polymorphism.
-- - Useful for performing similar operations (e.g., addition, converting to strings) on different types.

-- In Haskell, ad-hoc polymorphism is achieved through **typeclasses**:
-- - A typeclass specifies a set of operations that must be defined for a type.
-- - It allows us to work with a type `a`, as long as it supports certain operations, 
--   regardless of what `a` specifically is.

-- Example of a Typeclass:
class Show a where
  show :: a -> String

-- - Only values with a `show` function can be represented as strings.
-- - `show` converts values into `String` representations.

-- Typeclass constraints:
-- - We use typeclass constraints on the left-hand side of the big arrow (=>).
-- Example: 
showValue :: (Show a) => a -> String
showValue = show

-- Defining Typeclass Instances:
-- - We can declare a type to belong to a typeclass using two methods:
--   1. **Instance Declaration**: We explicitly define the implementations for each function.
--   2. **Deriving Clause**: Haskell can automatically generate default behavior for some typeclasses.

-- Example: Algebraic Data Type and Instance Declaration
data Insect = Spider | Centipede | Ant

-- Implementing the `Eq` typeclass manually:
instance Eq Insect where
  Spider == Spider = True
  Centipede == Centipede = True
  Ant == Ant = True
  _ == _ = False

-- Implementing the `Show` typeclass manually:
instance Show Insect where
  show Spider = "Spider"
  show Centipede = "Centipede"
  show Ant = "Ant"

-- Using Deriving for Typeclasses:
-- - We can use `deriving` to automatically generate default implementations for typeclasses like `Eq` and `Show`.
data Insect = Spider | Centipede | Ant
  deriving (Show, Eq)

-- Example:
-- Ant == Ant        -- True
-- Spider /= Ant     -- True
-- show Centipede    -- "Centipede"

-- Deriving vs. Instance:
-- - Use **deriving** when you want default behavior.
-- When the behaviour is already specified for any associated data
-- When the typeclass supports deriving


-- - Use **instance** when the typeclass doesn’t support deriving or you need custom behavior (e.g., a custom typeclass)
-- When you want behaviour that’s different to the default (e.g., you want 
-- Show to pretty-print an abstract syntax tree)


-- The `Read` Typeclass:
-- - `read` converts `String` values into other types (like input in Python or `scanf` in C).
data Insect = Spider | Centipede | Ant
  deriving (Read, Show, Eq)

-- Example: Reading a value as an `Insect`
-- (read "Centipede") :: Insect  -- Output : Centipede

-- Note: Type annotations are required when using `read`.

-- Custom Typeclass Example: The Leggy Typeclass
class Leggy a where
  numLegs :: a -> Int

-- Defining instances for `Leggy`:
instance Leggy Insect where
  numLegs Spider = 8
  numLegs Ant = 6
  numLegs Centipede = 100

-- Usage Example:
describe :: (Show a, Leggy a) => a -> String
describe x = (show x) ++ "s have " ++ (show $ numLegs x) ++ " legs"

-- Could this function have the type `describe :: Insect -> String`?
-- Yes, but using typeclasses makes the code more **extensible**. Now we can describe any instance of `Leggy`.

-- Example of Adding Another Type to the Leggy Typeclass:
data Mammal = Human | Dog | Cat | Pig
  deriving (Show, Eq)

instance Leggy Mammal where
  numLegs Human = 2
  numLegs _ = 4

-- Now we can describe mammals too:
-- describe Human  -- "Humans have 2 legs"
-- describe Dog    -- "Dogs have 4 legs"

-- Another Example: The Noisy Typeclass
class Noisy a where
  mkNoise :: a -> String

instance Noisy Mammal where
  mkNoise Dog = "woof"
  mkNoise Cat = "meow"
  mkNoise Pig = "oink"
  mkNoise _ = "ow"

-- Extending it to Insect types:
instance Noisy Insect where
  mkNoise Spider = "shhh"
  mkNoise Ant = "scritch"
  mkNoise Centipede = "rustle"

-- Putting It Together: 
-- Nursery rhyme with `song`
song :: (Show a, Noisy a) => [a] -> String
song animals = concatMap verse animals
  where verse x = "The " ++ show x ++ " goes " ++ mkNoise x ++ "\n"

-- Example usage:
putStrLn $ song [Pig, Dog, Cat]
Output:
The Pig goes oink
The Dog goes woof
The Cat goes meow

-- Ad-hoc polymorphism, through typeclasses, enables **type-safe code reuse** and allows us to handle similar operations across different types while keeping our code flexible and extensible.
