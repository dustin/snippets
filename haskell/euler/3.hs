-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the number 600851475143 ?

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

-- Prime factorize a number by checking divisibility with each number
-- in the above list.
factor :: Integer -> [Integer]
factor n = rfactor n primes
       where rfactor n2 (car:cdr)
              | car * car > n2  = [n2]
              | mod n2 car == 0 = car : rfactor (div n2 car) (car:cdr)
              | otherwise       = rfactor n2 cdr

euler3 n = last (factor n)