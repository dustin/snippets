{-# LANGUAGE OverloadedLists, TypeFamilies #-}

import Data.Monoid
import Data.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import GHC.Exts (IsList(..))
-- ZipList applicative

data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

instance Semigroup (List a) where
   Nil <> b = b
   Cons a xs <> ys = (Cons a (mappend xs ys))

instance Monoid (List a) where
  mempty = Nil

concat' :: List (List a) -> List a
concat' = fold mappend Nil

instance IsList (List a) where
  type Item (List a) = a

  fromList [] = Nil
  fromList (x:xs) = Cons x (fromList xs)

  toList Nil = []
  toList (Cons a l) = [a] <> toList l

flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f

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

-- The weekend starts here

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
            in take' 3000 l
          ys' = let (ZipList' l) = ys
            in take' 3000 l

instance Semigroup (ZipList' a) where
  (ZipList' Nil) <> b = b
  (ZipList' a) <> (ZipList' b) = ZipList' (a <> b)

instance Monoid (ZipList' a) where
  mempty = ZipList' Nil

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = ZipList' . fromList . repeat

  -- :: ZipList' (a -> b) -> ZipList' a -> ZipList' b
  (ZipList' Nil) <*> _ = ZipList' Nil
  _ <*> (ZipList' Nil) = ZipList' Nil
  (ZipList' (Cons f fs)) <*> (ZipList' (Cons v vs)) = ZipList' (Cons (f v) Nil) <> ((ZipList' fs) <*> (ZipList' vs))

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

-- Prelude> let zl' = ZipList'
-- Prelude> let z = zl' [(+9), (*2), (+8)]
-- Prelude> let z' = zl' [1..3]
-- Prelude> z <*> z'
-- ZipList' [10,4,11]
-- Prelude> let z' = zl' (repeat 1)
-- Prelude> z <*> z'
-- ZipList' [10,2,9]
