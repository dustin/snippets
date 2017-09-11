The decimal number, 585 = 1001001001(2) (binary), is palindromic in both
bases.

Find the sum of all numbers, less than one million, which are
palindromic in base 10 and base 2.

(Please note that the palindromic number, in either base, may not include leading zeros.)

> import Euler

A palindrome is a list that's equal to itself reversed.

> palindrome :: Eq t => [t] -> Bool
> palindrome a = a == (reverse a)

A number is a double-palindrome here if it's a palindrome in base 10 and base 2.

> dpalindrome x = (palindrome $ digitsb 10 x) && (palindrome $ digitsb 2 x)

> euler36 = sum $ [x | x <- [1..1000000], dpalindrome x]
