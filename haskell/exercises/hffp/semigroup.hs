module Main where

import Data.Semigroup

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine c1) <> (Combine c2) = Combine (c1 <> c2)

f :: Combine Integer (Sum Integer)
f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)

-- unCombine (f <> g) $ 0   == 0
-- unCombine (f <> g) $ 1   == 2
-- unCombine (f <> f) $ 1   == 4
-- unCombine (g <> f) $ 1   == 2


data Validation a b = Failure a | Success b
                    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  a@(Success _) <> _         = a
  _ <> b@(Success _)         = b
  (Failure a) <> (Failure b) = Failure (a <> b)
  a <> _                     = a

validatevalidation = do
  let failure :: String -> Validation String Int
      failure = Failure
      success :: Int -> Validation String Int
      success = Success
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2
