The sequence of triangle numbers is generated by adding the natural
numbers.  So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28.
The first ten terms would be:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

Let us list the factors of the first seven triangle numbers:

 1: 1
 3: 1,3
 6: 1,2,3,6
10: 1,2,5,10
15: 1,3,5,15
21: 1,3,7,21
28: 1,2,4,7,14,28

We can see that 28 is the first triangle number to have over five divisors.

What is the value of the first triangle number to have over five hundred divisors?

------------------------------------------------------------------------------

> import Data.List

Let's start with a list of all triangle numbers.

> triangles = [sum [1..x] | x <- [1..]]

And a factorizor, which needs an integer square root to avoid doing entirely too much math.

> isqrt :: Integer -> Integer
> isqrt x = ceiling $ sqrt $ fromIntegral x

Our factor generator doesn't produce a sorted list like the above
because it only computes up to the square root to find divisors, and
then throws away the duplicates as it unions the two bits together.

> factor n = let lower = [x | x <- [1..isqrt n], n `mod` x == 0] in
>              union lower (map (div n) lower)

Here's a function that just spits out numbers that have at least n factors.

> big_triangles n = [x | x <- triangles, length (factor x) > n]

Since we just one the first, we'll pop it off the infinite list above:

> euler12 = head $ big_triangles 500