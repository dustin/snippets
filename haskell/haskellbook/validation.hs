import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers (quickBatch, EqProp(..), eq)
import Test.QuickCheck.Classes (functor, applicative)

data Validation e a =
  Failure e
  | Success a
  deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
  fmap _ (Failure a) = Failure a
  fmap f (Success a) = Success (f a)

-- This is different
instance Monoid e => Applicative (Validation e) where
  pure = Success
  Success f <*> Success v = Success (f v)
  Success _ <*> Failure e = Failure e
  Failure e <*> Success _ = Failure e
  Failure e <*> Failure e' = Failure (e <> e')

instance (Eq a, Eq e) => EqProp (Validation e a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where
  arbitrary = frequency [
    (1, Success <$> arbitrary),
    (1, Failure <$> arbitrary)
    ]
