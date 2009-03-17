-- 2520 is the smallest number that can be divided by each of the
-- numbers from 1 to 10 without any remainder.

-- What is the smallest number that is evenly divisible by all of the
-- numbers from 1 to 20?

divisible_by :: Integer -> [Integer] -> Bool

-- Empty list as true is a little weird here...
divisible_by n [] = True
divisible_by n (h:tl)
             | n `mod` h == 0 = divisible_by n tl
             | otherwise = False

euler_5 :: Integer -> Integer

-- This method does not work in any reasonable amount of time.  Turns
-- out, that's a pretty large search space.  The thing it's asking for
-- is the LCM across a bunch of numbers, and it turns out that haskell
-- has an lcm function for two numbers...
-- euler_5 n = [x | x <- [1..], divisible_by x [1..n]] !! 0

euler_5 n = foldl lcm 1 [2..n]