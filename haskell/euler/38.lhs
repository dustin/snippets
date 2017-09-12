Take the number 192 and multiply it by each of 1, 2, and 3:

192 × 1 = 192
192 × 2 = 384
192 × 3 = 576

By concatenating each product we get the 1 to 9 pandigital,
192384576. We will call 192384576 the concatenated product of 192 and
(1,2,3)

The same can be achieved by starting with 9 and multiplying by 1, 2,
3, 4, and 5, giving the pandigital, 918273645, which is the
concatenated product of 9 and (1,2,3,4,5).

What is the largest 1 to 9 pandigital 9-digit number that can be
formed as the concatenated product of an integer with (1,2, ... , n)
where n > 1?


> import Data.List
> import Euler
> import Data.Maybe

> pandigital x = is_pandigital_n 9

This basically is the forward function.  Given a base number and
sequence of numbers, return the number you get from concatenating the
products of that number and each number in the sequence.

> seqprod :: Integer -> [Integer] -> Integer
> seqprod n nums = undigits $ concatMap (\x -> digits $ x * n) nums

We already know one fairly big answer from the question, so let's not
consider any smaller than that, starting with the biggest numbers, and
then working our way down.

> totry = (reverse.sort) [ undigits x | x <- permutations (digits 123456789),
>                         undigits x >= 918273645]

This function generates the sequence (or nothing).

> mkseq :: Integer -> Maybe (Integer, Integer)
> mkseq i = go (digits i) [1..5]
>   where go d [] = Nothing
>         go d (l:ls) = let n = undigits $ take l d
>                           s = takeWhile (\x -> seqprod n [1..x] <= i) [1..] in
>                         if seqprod n s == i then Just (n, last s) else go d ls

This is a list of all of them.

> pandigitals = mapMaybe mkseq totry

euler 30 just wants the first

> euler_30 = let (l,r) = head pandigitals in seqprod l [1..r]

