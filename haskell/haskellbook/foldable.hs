import Prelude hiding (sum, product, elem, minimum, maximum, null, length, toList)
import Data.Monoid
import Data.Foldable (fold, foldMap)

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem n = foldr (\x o -> if x == n then True else o) False

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum l = foldr (\x o -> min' (Just x) o) Nothing l
  where min' x Nothing = x
        min' x o = min x o

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum l = foldr (\x o -> max' (Just x) o) Nothing l
  where max' x Nothing = x
        max' x o = max x o

null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

length :: (Foldable t) => t a -> Int
length = foldr (\_ o -> o + 1) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x o -> f x <> o) mempty


-- ----------------------------------------------------------------------

data Constant a b =
  Constant b

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b


data Two a b =
  Two a b

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ b c) = f b <> f c

-- ----------------------------------------------------------------------

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldr (\x o -> if (f x) then pure x <> o else o) mempty
