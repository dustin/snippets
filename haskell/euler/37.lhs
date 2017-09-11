The number 3797 has an interesting property.  Being prime itself, it
is possible to continuously remove digits from left to right, and
remain prime at each stage: 3797, 797, 97, and 7. Similarly we can
work from right to left: 3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from
left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.



> import Data.List

Copied a bunch of stuff from 35.

> digits :: Integer -> [Integer]
> digits 0 = [0]
> digits n = reverse $ go n
>   where go 0 = []
>         go x = let (a,b) = x `divMod` 10 in b : go a

primes code from 27

> isPrime :: Integer -> Bool
> isPrime n
>   | n < 2 = False
>   | otherwise = not $ any (\x -> n `mod` x == 0) $ takeWhile (\c -> c^2 <= n) primes

Then a list of all primes:

> primes :: [Integer]
> primes = 2:[x | x <- [3,5..], isPrime x]

undigit is basically the oposite of digits.  e.g. [1, 2, 3] -> 123

> undigit :: [Integer] -> Integer
> undigit = foldl (\b x -> b * 10 + x) 0

Truncateable primes are primes are prime and the left and right
truncations are prime.

> truncatable p = isPrime p && left p && right p
>   where left 0 = True
>         left x = isPrime x && left (undigit $ tail $ digits x)
>         right 0 = True
>         right x = isPrime x && right (x `div` 10)

We know there are only eleven, so just start digging through all prime
numbers until we've got 11 and sum them up.

> euler_37 = sum $ take 11 $ filter truncatable $ dropWhile (<10) primes
