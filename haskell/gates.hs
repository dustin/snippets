import Prelude hiding (not, and, or, xor)
import Control.Monad (guard, mapM_)

printTable :: (Bool -> Bool -> Bool) -> IO ()
printTable f = mapM_ print $ do
  a <- [False, True]
  b <- [False, True]
  pure (a, b, f a b)

printTable2 :: (Bool -> Bool -> Bool -> Bool) -> IO ()
printTable2 f = mapM_ print $ do
  a <- [False, True]
  b <- [False, True]
  c <- [False, True]
  pure (a, b, c, f a b c)


nand True True = False
nand _ _ = True

not a = nand a a

and a b = not $ nand a b

or a b = nand (not a) (not b)

xor a b = or (and a (not b)) (and b (not a))

-- if (s == 0) a else b
-- stolen from https://www.electronics-tutorials.ws/combination/comb_2.html
mux s a b = nand (nand a (not s)) (nand b s)
