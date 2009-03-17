-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we
-- can see that the 6^(th) prime is 13.

-- What is the 10001^(st) prime number?

-- I stole this prime list from one of my first attempts to do
-- something in haskel.
isPrime :: Integer -> Bool

isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime x
        | even(x)   = False
	| otherwise = rprime 3
	where rprime a
                | (a ^ 2) > x    = True
                | mod x a == 0   = False
                | otherwise      = rprime (a + 2)

-- All prime numbers
primes = [ x | x <- [2..], isPrime x ]

euler_7 :: Int -> Integer

-- Almost curried this, were it not for the stupid 0-base
euler_7 n = primes !! (n - 1)