The definition of the problem from project euler looks like this:

    A Pythagorean triplet is a set of three natural numbers,
    a < b < c, for which, a^2 + b^2 = c^2

    For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

    There exists exactly one Pythagorean triplet for which a + b + c = 1000.
    Find the product abc.

> is_triplet :: Int -> Int -> Int -> Bool
> is_triplet a b c = a < b && b < c && (a ^ 2) + (b ^ 2) == (c ^ 2)

The built-in sqrt only works with floating point numbers.  I
introduced an integer square root that returns 0 if a number isn't a
proper square.  This fits well in the Pythagorean triplet thing we're
doing because 0 will always be less than b.

> isqrt :: Int -> Int
> isqrt x = let ds = fromEnum $ sqrt $ fromIntegral x in if ds^2 == x then ds else 0

Next, a list of all of the triplets whose sum of a+b <= m.  This
overshoots, but considering c would like, take up more space on the screen.

> triplets s = [(a, b, c) | a <- [1..s], b <- [a+1..s], c <- [isqrt $ (a^2) + (b^2)], c > b]

Search for triplets where a+b+c = s.

> search s = filter (\(a,b,c) -> a+b+c == s) $ triplets s

For euler9 in particular, there's allegedly exactly one case where
a+b+c=1000, so I'll take one so that we stop as soon as we find the
first one.

> euler9 = take 1 $ search 1000
