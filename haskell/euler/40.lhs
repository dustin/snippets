An irrational decimal fraction is created by concatenating the
positive integers:

0.123456789101112131415161718192021...

It can be seen that the 12th digit of the fractional part is 1.

If dn represents the nth digit of the fractional part, find the value
of the following expression.

d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000


Should be easy enough to produce a string of all numbers...

> import Data.List

I initially prefixed this with ('0':'.'), but all of the interesting
offsets are from the fractional part only, so let's just get the
digits.  I still added the . just to avoid being off by one since !!
is zero based.

> num = '.' : concatMap show [1..]

Then we just need the d definition.

> d :: Int -> Int
> d = (flip (-) 48) . fromEnum.(!!) num

And multiply the interesting digits together.

> euler_40 = foldr (\o b -> b * (d $ 10^o)) 1 [0..6]
