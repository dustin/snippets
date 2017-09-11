145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the
factorial of their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.


digits code from 33

> digits :: Integer -> [Integer]
> digits 0 = [0]
> digits n = reverse $ go n
>   where go 0 = []
>         go x = let (a,b) = x `divMod` 10 in b : go a

> fact a = product [1..a]

> curious x = x == sum (map fact (digits x))

I estimated the upper bound.  Probably could've come up with a better
explanation to what the upper bounds could be here, but I've got work to do.

> euler_34 = sum [x | x <- [3..100000], curious x]
