{-# LANGUAGE InstanceSigs #-}

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

data Die =
  DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    -- Use 'error'
    -- _extremely_ sparingly.
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise = let (die, nextGen) = randomR (1, 6) gen
                    in go (sum + die) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n = count
      | otherwise = let (die, nextGen) = randomR (1, 6) gen
                    in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go [] g
  where go :: [Int] -> StdGen -> (Int, [Die])
        go l gen
          | sum l >= n = (sum l, map intToDie l)
          | otherwise = let (die, nextGen) = randomR (1, 6) gen
                        in go (die:l) nextGen

-- ----------------------------------------------------------------------

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> let (a,s') = g s in (f a, s')

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s -> let (f',s') = f s
                                        (b,s'') = g s' in (f' b, s'')

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s -> let (a,s') = f s
                                  (Moi g') = g a in
                                g' s'

-- ----------------------------------------------------------------------

get' :: Moi s s
get' = Moi $ \s -> (s,s)

put' :: s -> Moi s ()
put' s = Moi $ \_ -> ((), s)

-- Prelude> runState (put "blah") "woot"
-- ((),"blah")

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd (sa s)

eval :: Moi s a -> s -> a
eval (Moi sa) s = fst (sa s)

moidify :: (s -> s) -> Moi s ()
moidify f = Moi $ \s -> ((), f s)
