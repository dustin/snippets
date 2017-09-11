The number 3797 has an interesting property.  Being prime itself, it
is possible to continuously remove digits from left to right, and
remain prime at each stage: 3797, 797, 97, and 7. Similarly we can
work from right to left: 3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from
left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.


> import Euler
> import Data.List

Truncateable primes are primes are prime and the left and right
truncations are prime.

> truncatable p = isPrime p && left p && right p
>   where left 0 = True
>         left x = isPrime x && left (undigits $ tail $ digits x)
>         right 0 = True
>         right x = isPrime x && right (x `div` 10)

We know there are only eleven, so just start digging through all prime
numbers until we've got 11 and sum them up.

> euler_37 = sum $ take 11 $ filter truncatable $ dropWhile (<10) primes
