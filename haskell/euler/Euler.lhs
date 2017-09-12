As I've been going through various euler things, I've noticed I've
been copying and pasting a bunch of code around.  I decided to make a
euler module that contains the stuff I've been copying and pasting.

> module Euler where

> import Data.List (union, sort)

digitsb takes an integer and returns a list of the digits of that
integer in base b.

> digitsb :: Int -> Integer -> [Integer]
> digitsb _ 0 = []
> digitsb b n = reverse $ go n
>   where go 0 = []
>         go x = let (a, r) = x `divMod` (toEnum b) in r : go a

digits is a base 10 version of digitsb.  Several of the euler puzzles
so far have needed the digits in base 10, so that's a first class
function here.  e.g., 123 -> [1, 2, 3]

> digits = digitsb 10

undigits is just the opposite of digits.  e.g., [1, 2, 3] -> 123

> undigits :: [Integer] -> Integer
> undigits = foldl (\b x -> b * 10 + x) 0

There are also quite a lot of puzzles that use prime numbers.

> isPrime :: Integer -> Bool
> isPrime n
>   | n < 2 = False
>   | otherwise = not $ any (\x -> n `mod` x == 0) $ takeWhile (\c -> c^2 <= n) primes

Then a list of all primes:

> primes :: [Integer]
> primes = 2:[x | x <- [3,5..], isPrime x]

Also end up needing factorial a lot.

> fact :: Integer -> Integer
> fact a = product [1..a]

I've needed the integer square root a bunch -- mostly for computing
divisors, or finding similar pivot points.

> isqrt :: Integer -> Integer
> isqrt x = ceiling $ sqrt $ fromIntegral x

This is the "proper" divisors -- all of the divisors other than the number itself.

> divisors 1 = [1]
> divisors 2 = [1]
> divisors n = let lower = [x | x <- [2..isqrt n], n `mod` x == 0] in
>                1 : union lower (map (div n) lower)

This is all the factors (including the number).

> factor n = let lower = [x | x <- [1..isqrt n], n `mod` x == 0] in
>              union lower (map (div n) lower)

We also want a prime factorizing function.  This is a slightly
different form and much older code (from years ago when I wrote 3.hs).
It's a good deal faster than the naïve list comprehension or filter
version.

> factor' :: Integer -> [Integer]
> factor' n = rfactor n primes
>   where rfactor n2 (car:cdr)
>           | car * car > n2  = [n2]
>           | mod n2 car == 0 = car : rfactor (div n2 car) (car:cdr)
>           | otherwise       = rfactor n2 cdr

I've also needed the fibonacci sequence a few times.  Let's add one of
those.  This one starts with 1.  Sometimes I start with zero.  I can
always stick a 0 in front of it.

> fib = 1:1: (zipWith (+) fib $ tail fib)

There are a bunch of questions about pandigital numbers.  Let's try to consolidate this.

> is_pandigital_n n x = sort (digits x) == [1..n]

> is_pandigital x = let d = digits x in sort d == [1..(toEnum . length) d]

