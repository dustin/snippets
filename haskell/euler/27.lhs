Euler discovered the remarkable quadratic formula:

n^2+n+41

It turns out that the formula will produce 40 primes for the
consecutive integer values 0≤n≤39. However, when
n=40,40^2+40+41=40(40+1)+41 is divisible by
41, and certainly when n=41,41^2+41+41 is clearly
divisible by 41.

The incredible formula n^2−79n+1601 was discovered, which produces 80
primes for the consecutive values 0≤n≤79. The product of the
coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

n^2+an+b, where |a|<1000 and |b|≤1000

where |n| is the modulus/absolute value of n
e.g. |11|=11 and |−4|=4

Find the product of the coefficients, a and b, for the quadratic
expression that produces the maximum number of primes for consecutive
values of n, starting with n=0.

> import Data.List (intersect)
> import Euler

Primes from 10, modified to deal with negative numbers.

This is kind of lame, but I didn't want to type a lot.  Just generate
the list of primes for n from 0 to ∞ and then count them.  I can
implement a `foldWhile` if this is too resource intensive someday, but
there's not that much work to do here.

> numprimes :: Integer -> Integer -> Int
> numprimes a b = length $ takeWhile (isPrime . euquad a b) [0..]

The basic skeleton of the function as defined above.

> euquad :: Num a => a -> a -> a -> a
> euquad a b n = n^2 + (a * n) + b

The interesting set of numbers is 0, 1, all of the primes < 1000 and
negatives of all of the above.

> interesting = let p = takeWhile (<1000) primes in map (0 -) p ++ (-1:0:1:p)

Then we just fold over every pair of interesting numbers keeping the
ones that generated the longest sequence of primes.

> euler27 = foldr (\(a,b) m@(_,_,n') -> let n = numprimes a b in if n > n' then (a,b,n) else m)
>                 (0,0,0) [(a, b) | a <- interesting, b <- interesting]
