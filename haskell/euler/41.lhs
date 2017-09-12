We shall say that an n-digit number is pandigital if it makes use of
all the digits 1 to n exactly once. For example, 2143 is a 4-digit
pandigital and is also prime.

What is the largest n-digit pandigital prime that exists?

> import Euler
> import Data.List

Well, we know it can't be larger than 9 digits, so let's start with
just the list of primes < 9 digits long:

> candidates = reverse $ takeWhile (<1000000000) primes

... ok, that's a bad idea.  It takes me too long to generate all those
primes.  However, my prime validation code only needs to generate up
to about the square root of those when checking pandigital primes, so
let's just generate all the pandigital numbers.

> pandigitals = (reverse.sort) $ map undigits $ concatMap permutations [[1..n] | n <- [4..9]]

The answer is just the first one of those.

> euler41 = head $ filter isPrime pandigitals
