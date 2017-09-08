A perfect number is a number for which the sum of its proper divisors
is exactly equal to the number. For example, the sum of the proper
divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28
is a perfect number.

A number n is called deficient if the sum of its proper divisors is
less than n and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the
smallest number that can be written as the sum of two abundant numbers
is 24. By mathematical analysis, it can be shown that all integers
greater than 28123 can be written as the sum of two abundant
numbers. However, this upper limit cannot be reduced any further by
analysis even though it is known that the greatest number that cannot
be expressed as the sum of two abundant numbers is less than this
limit.

Find the sum of all the positive integers which cannot be written as
the sum of two abundant numbers.


Grabbing my divisors code from 12:

> import qualified Data.List as L
> import qualified Data.Set as Set

> isqrt :: Integral n => n -> n
> isqrt x = ceiling $ sqrt $ fromIntegral x

> divisors 1 = [1]
> divisors 2 = [1]
> divisors n = let lower = [x | x <- [2..isqrt n], n `mod` x == 0] in
>                1 : L.union lower (map (div n) lower)

We don't need perfect numbers here, so let's just consider the abundant ones.

> is_abundant n = n < (sum.divisors) n

Which gives us a list of all abundant numbers.

> maxnum = 28124

> abundants = [x | x <- [12..maxnum], is_abundant x]

From which we can create a list of all numbers that are the sum of two abundant numbers:

> sumbundants = Set.fromList [x+y | x <- abundants, y <- dropWhile (< x) abundants, x+y <= maxnum]

And then we can just like, subtract the set of those from all numbers.

> non_abundant = (Set.fromAscList [1..maxnum]) Set.\\ sumbundants

And the final answer.

> euler_23 = Set.foldl (+) 0 non_abundant
