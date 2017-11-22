module Main where

import Data.Monoid
import Test.QuickCheck

-- Monoid tests

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend (Mem f1) (Mem f2) = Mem $ \v ->
                                      let (s1, a1) = f1 v
                                          (s2, a2) = f2 a1 in
                                        (s1 <> s2, a2)

f' :: Mem Int String
f' = Mem $ \s -> ("hi", s + 1)

main = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft                    -- ("hi", 1)
  print $ rmright                   -- ("hi", 1)
  print $ (rmzero :: (String, Int)) -- ("", 0)
  print $ rmleft == runMem f' 0     -- True
  print $ rmright == runMem f' 0    -- True
