import Data.Monoid
import Data.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

-- ----------------------------------------------------------------------

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Show, Eq)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap f (Constant a) = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = pure (Constant a)

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where (=-=) = eq

-- ----------------------------------------------------------------------

data Optional a =
  Nada
  | Yep a
  deriving (Show, Eq)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Applicative Optional where
  pure = Yep
  Nada <*> _ = Nada
  Yep f <*> o = f <$> o

instance Foldable Optional where
  foldMap f Nada = mempty
  foldMap f (Yep v) = f v

instance Traversable Optional where
  traverse f Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(3, Yep <$> arbitrary), (1, pure Nada)]

instance Eq a => EqProp (Optional a) where (=-=) = eq

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

instance Foldable List where
  foldr _ o Nil = o
  foldr f o (Cons x xs) = f x (foldr f o xs)

instance Traversable List where
  -- sequenceA :: Applicative f => t (f a) -> f (t a)
  -- e.g.    List (Maybe Int)  ->   Maybe (List Int)
  sequenceA Nil = pure Nil
  sequenceA (Cons x xs) = Cons <$> x <*> sequenceA xs

-- ----------------------------------------------------------------------

data Three a b c =
  Three a b c
  deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

-- ----------------------------------------------------------------------

data Pair a b =
  Pair a b
  deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where (=-=) = eq

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

-- ----------------------------------------------------------------------

data Big a b =
  Big a b b
  deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where (=-=) = eq

instance Functor (Big a) where
  fmap f (Big a b c) = Big a (f b) (f c)

instance Foldable (Big a) where
  foldMap f (Big a b c) = mconcat [f b, f c]

instance Traversable (Big a) where
  traverse f (Big a b c) = Big a <$> f b <*> f c

-- ----------------------------------------------------------------------

data Bigger a b =
  Bigger a b b b
  deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where (=-=) = eq

instance Functor (Bigger a) where
  fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d)

instance Foldable (Bigger a) where
  foldMap f (Bigger a b c d) = mconcat [f b, f c, f d]

instance Traversable (Bigger a) where
  traverse f (Bigger a b c d) = Bigger a <$> f b <*> f c <*> f d

-- ----------------------------------------------------------------------

data Tree a =
  Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- foldMap is a bit easier
-- and looks more natural,
-- but you can do foldr too
-- for extra credit.
instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

instance Traversable Tree where
  sequenceA Empty = pure Empty
  sequenceA (Leaf a) = Leaf <$> a
  sequenceA (Node l a r) = Node <$> sequenceA l <*> a <*> sequenceA r
