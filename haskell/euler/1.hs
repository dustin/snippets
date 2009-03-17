-- If we list all the natural numbers below 10 that are multiples of 3 or
-- 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
--
-- Find the sum of all the multiples of 3 or 5 below 1000.

multiple_of_3_or_15 :: Integer -> Bool

multiple_of_3_or_15 x = x `mod` 3 == 0 || x `mod` 5 == 0

multiple_of_any :: Integer -> [Integer] -> Bool

multiple_of_any x = any (\ n -> x `mod` n == 0)

euler1 n = sum [ x | x <- [0..n], multiple_of_3_or_15 x]

euler1_b n = sum [ x | x <- [0..n], multiple_of_any x [3, 5]]
