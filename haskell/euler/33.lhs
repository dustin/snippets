The fraction 49/98 is a curious fraction, as an inexperienced
mathematician in attempting to simplify it may incorrectly believe
that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction,
less than one in value, and containing two digits in the numerator and
denominator.

If the product of these four fractions is given in its lowest common
terms, find the value of the denominator.


> import Data.List

Instead of reusing that digits code from 16, 20, etc... I made a
slightly less hacky one.

> digits :: Integer -> [Integer]
> digits 0 = [0]
> digits n = reverse $ go n
>   where go 0 = []
>         go x = let (a,b) = x `divMod` 10 in b : go a

divisors from 12

> isqrt :: Integer -> Integer
> isqrt x = ceiling $ sqrt $ fromIntegral x
>
> factor n = let lower = [x | x <- [1..isqrt n], n `mod` x == 0] in
>              union lower (map (div n) lower)

We need to know if to two numbers have a common digit.  We've filtered
out the "trivial" values -- those with zeros, so we'll just return
zero as the common digit in the case where there isn't one.  This
makes it easier to work with later.

> common_digit a b
>   | i == [] = 0
>   | otherwise = head i
>   where i = intersect (digits a) (digits b)

> has_common_digit a b = common_digit a b /= 0

A basic GCF so we can reduce fractions.

> gcf a b = maximum $ intersect (factor a) (factor b)
>
> reduce a b = (a `div` g, b `div` g)
>   where g = gcf a b

This is our definition of the fancy digit canceling thing.  If doing
the digit canceling gives us the same reduced answer as regular
reduction, it's what we're looking for.

> digit_canceling a b = (reduce a b) == (reduce a' b')
>   where a' = dc a
>         b' = dc b
>         dc x = head $ filter (\c -> c /= common_digit a b) $ digits x

So then we just make a list of all of the numbers, less than one, that
meet our requirements.  Starting the denominator from (succ d) keeps
us less than one.  Filter out the "d0" and "dd" and then just match
our labels.

> oddities = [(n,d) | n <- [11..99], d <- [succ n..99],
>                     n `mod` 10 /= 0, d `mod` 10 /= 0, -- trivial
>                     n `mod` 11 /= 0, d `mod` 11 /= 0, -- repeating digits
>                     has_common_digit n d, digit_canceling n d]

The final answer is just the denominator of the reduced fraction of
all of the fractions multiplied together.  I return the whole fraction here.

> euler_33 = uncurry reduce $ foldr (\(n,d) (n',d') -> (n*n', d*d')) (1,1) oddities
