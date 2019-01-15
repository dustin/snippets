{-# LANGUAGE InstanceSigs #-}

import Data.Char
import Control.Applicative

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = do
  x <- rev
  y <- cap
  pure (x, y)

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap f (Reader r) = Reader (f . r)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader (const a)

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> (rab r . ra) r

-- ----------------------------------------------------------------------

instance Monad (Reader r) where
  return = pure

  -- Expected type: Reader r b
  -- Actual type: Reader r (Reader r b)

  -- Reader r (Reader r b) is just (r -> r -> b)

  -- r -> (r -> b) -> (r -> b)

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

  -- (r -> a)    a -> (r -> b)

-- ----------------------------------------------------------------------
newtype HumanName = HumanName String
  deriving (Eq, Show)
newtype DogName = DogName String
  deriving (Eq, Show)
newtype Address = Address String
  deriving (Eq, Show)

data Person = Person {
  humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog = Dog {
  dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird")
       (DogName "Barkley")
       (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen")
        (DogName "Papu")
        (Address "Austin")

-- without Reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogRM :: Reader Person Dog
getDogRM = ask >>= pure . (Dog <$> dogName <*> address)
