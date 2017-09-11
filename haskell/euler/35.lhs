The number, 197, is called a circular prime because all rotations of
the digits: 197, 971, and 719, are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

How many circular primes are there below one million?


> import Data.List
> import Euler

Given a list, return all of the rotations of a list.  Sort of an
order-preserving permutation code:  e.g.:   "abc" -> ["bca", "cab", "abc"]

> rotate :: [t] -> [[t]]
> rotate l = take (length l) $ go l
>   where go (x:xs) = (xs ++ [x]) : go (xs++[x])

List all of the rotations of digits for a number.

> circle a = map undigits $ rotate $ digits a

A number is a circular prime if all rotations of its digits are prime.

> circular_prime :: Integer -> Bool
> circular_prime a = all isPrime (circle a)

Count them up.

> euler_35 = length $ filter circular_prime $ takeWhile (<1000000) primes
