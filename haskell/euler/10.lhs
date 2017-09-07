euler 10:

    The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

    Find the sum of all the primes below two million.

First, a function to determine if a number is prime:

> isPrime :: Integer -> Bool
> isPrime n = not $ any (\x -> n `mod` x == 0) $ takeWhile (\c -> c^2 <= n) primes

Then a list of all primes:

> primes = 2:[x | x <- [3,5..], isPrime x]

Do the computation:

> euler10 = sum $ takeWhile (< 2000000) primes
