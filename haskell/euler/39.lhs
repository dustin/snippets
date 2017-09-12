If p is the perimeter of a right angle triangle with integral length
sides, {a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?


pythagorean theorem says a^2 + b^2 = c^2
We want to find a,b,c where a^2 + b^2 is a square and a+b+c = n

> import Data.List
> import Euler

> isqrt' x = let s = isqrt x in if s * s == x then Just s else Nothing

Test whether a,b,c is a triangle.

> is_triangle a b c = case isqrt' (a^2 + b^2) of
>                       Nothing -> False
>                       Just x -> c == x

Generate all of the possible integer lengths for a right triangle with
the given perimeter.

> lgen n = [(a,b,n - (a + b)) | a <- sort [1..n], b <- [a..(n-a)], is_triangle a b (n - (a+b))]

> lengths = (reverse.sort) [ (length (lgen x), x) | x <- [120..1000] ]

> euler_39 = head lengths

