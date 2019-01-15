import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Control.Monad (join, forM)

-- Write bind in terms of fmap and join.

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- join :: Monad m => m (m a) -> m a

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []


data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second a) = Second (f a)

instance Applicative (Sum a) where
  pure = Second
  (First x) <*> _ = First x
  _ <*> (First x) = First x
  (Second f) <*> (Second x) = Second (f x)

instance Monad (Sum a) where
  return = pure

  Second x >>= f = f x
  First x >>= _ = First x

instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = frequency [(3, First <$> arbitrary), (1, Second <$> arbitrary)]

-- ----------------------------------------------------------------------

data Nope a = NopeDotJpg deriving (Eq, Show)

instance (Eq a) => EqProp (Nope a) where (=-=) = eq

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure = const NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = pure NopeDotJpg

-- ----------------------------------------------------------------------

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

-- ----------------------------------------------------------------------

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
  fs <*> xs = flatMap (\f -> flatMap (pure . f) xs) fs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    n <- frequency [(3, arbitrary), (1, pure Nil)]
    pure $ Cons x n

instance (Eq a) => EqProp (List a) where (=-=) = eq

instance Monad List where
  Nil >>= _ = Nil
  l >>= f = concat' $ f <$> l

-- ----------------------------------------------------------------------

j :: Monad m => m (m a) -> m a
j m = m >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = f <$> m

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = f <$> a <*> b

-- This is really just forM
-- forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh l f = foldr (\x o -> f x >>= \i -> o >>= \o' -> pure (i : o')) (pure []) l

-- And this is sequence
-- sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id
