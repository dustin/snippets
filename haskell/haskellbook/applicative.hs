import Data.Monoid
import Data.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative (liftA3)

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = pure (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) b = fmap f b

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = pure <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq


newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  -- fmap f (Constant a) = Constant (f a)
  fmap _ (Constant a) = (Constant a)

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant a) (Constant b) = Constant (a <> b)

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where (=-=) = eq


data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold mappend Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f

instance Semigroup (List a) where
   Nil <> b = b
   Cons a xs <> ys = (Cons a (mappend xs ys))

instance Monoid (List a) where
  mempty = Nil

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure = flip Cons Nil

  -- There are two possibilities here that are both fine.  The first
  -- is the first I came up with.  The second is where I think the
  -- book was trying to take me

  -- fs <*> xs = fold (\f o -> (f <$> xs) <> o) Nil fs
  fs <*> xs = flatMap (\f -> flatMap (pure . f) xs) fs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    n <- frequency [(3, arbitrary), (1, pure Nil)]
    pure $ Cons x n

instance (Eq a) => EqProp (List a) where (=-=) = eq


-- 17.9 stuff

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
-- combos a b c = liftA3 (\a' b' c' -> (a',b',c')) a b c
-- combos = liftA3 (\a' b' c' -> (a',b',c'))
combos = liftA3 (,,)
