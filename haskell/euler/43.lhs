The number, 1406357289, is a 0 to 9 pandigital number because it is
made up of each of the digits 0 to 9 in some order, but it also has a
rather interesting sub-string divisibility property.

Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:

d2d3d4=406 is divisible by 2
d3d4d5=063 is divisible by 3
d4d5d6=635 is divisible by 5
d5d6d7=357 is divisible by 7
d6d7d8=572 is divisible by 11
d7d8d9=728 is divisible by 13
d8d9d10=289 is divisible by 17

Find the sum of all 0 to 9 pandigital numbers with this property.


> import Data.List
> import Euler

Well, this pandigital is a bit different from the other ones I've done
because it includes zero.  I guess I'll just do a function for that
here.

> is_panz x = (sort (digits x) == [0..9]) || (sort (0:digits x) == [0..9])

Chop the number up as specified above.  I'm not 100% sure how leading
zero works.

> chop :: Integer -> [Integer]
> chop x = [undigits $ (take 3.drop n) $ digits x | n <- [1..7]]

This validates a number against the properties above.

> seqdiv x = all (\(a,p) -> a `mod` p == 0) $ zip (chop x) primes

Generate a list of all of the 0-9 pandigitals.

> pans = map undigits $ permutations [0..9]

And limit that list to just the things that match our property.

> interesting = filter seqdiv pans

Answer is the sum of all of them.

> euler_43 = sum interesting


The above solution is slower than I'd like.  I'm going to try
rearranging a bit and see if I can get a fewer conversions to make a
difference.

> chop' :: [Integer] -> [Integer]
> chop' x = [undigits $ (take 3.drop n) $ x | n <- [1..7]]

> seqdiv' :: [Integer] -> Bool
> seqdiv' x = all (\(a,p) -> a `mod` p == 0) $ zip (chop' x) primes

> interesting' = filter seqdiv' $ permutations [0..9]

> euler_43' = foldr ((+) . undigits) 0 interesting'

euler_43' is over 5x faster than euler_43.  It finishes in ~18s
instead of over a minute and a half.
