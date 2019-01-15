{-# LANGUAGE FlexibleContexts #-}

module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = S (n a) a
  deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

-- instance (Applicative n, Testable (n Property), EqProp a) => EqProp (S n a) where
--   (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)

instance (Eq (n a), Eq a) => EqProp (S n a) where (=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S n a) = S (f <$> n) (f a)

instance (Foldable n, Functor n) => Foldable (S n) where
  foldMap f (S n a) = foldr (\x o -> f x <> o) (f a) n

instance Traversable n => Traversable (S n) where
  traverse = undefined

main = sample' (arbitrary :: Gen (S [] Int))
