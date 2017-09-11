The decimal number, 585 = 1001001001(2) (binary), is palindromic in both
bases.

Find the sum of all numbers, less than one million, which are
palindromic in base 10 and base 2.

(Please note that the palindromic number, in either base, may not include leading zeros.)



The digits for a given base.

> digbase :: Integer -> Integer -> [Integer]
> digbase 0 _ = []
> digbase n b = reverse $ go n
>   where go 0 = []
>         go x = let (a, r) = x `divMod` b in r : go a

A palindrome is a list that's equal to itself reversed.

> palindrome :: Eq t => [t] -> Bool
> palindrome a = a == (reverse a)

A number is a double-palindrome here if it's a palindrome in base 10 and base 2.

> dpalindrome x = (palindrome $ digbase x 10) && (palindrome $ digbase x 2)

> euler36 = sum $ [x | x <- [1..1000000], dpalindrome x]
