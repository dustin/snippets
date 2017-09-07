If the numbers 1 to 5 are written out in words: one, two, three, four, five,
then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written
out in words, how many letters would be used?


NOTE: Do not count spaces or hyphens. For example, 342 (three hundred
and forty-two) contains 23 letters and 115 (one hundred and fifteen)
contains 20 letters. The use of "and" when writing out numbers is in
compliance with British usage.


> import Data.Char
> import Data.List

First, we're going to need some words to name these things.

> ones =  ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
> teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
>          "seventeen", "eighteen", "nineteen"]
> tens =  ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

The numwords implementation isn't perfect -- there's some extraneous
space and it doesn't handle 0 in particular, but it produces enough
correct values to solve the euler problem.  It also doesn't hyphenate
the tens.

The use of "and" is particularly annoying to me.  I suspect it was
added just to complicate the problem a bit.  I implemented up to
999,999 using the same logic, so "and" gets added in the hundred
thousands place as well.  I dont know if this is standard since it's
hard to find standards.  I don't see it mentioned here:
http://www.grammarbook.com/numbers/numbers.asp (in fact, that says
it's not necessary, though British usage was mentioned in particular).

> numwords n
>   | n < 10 = ones !! n
>   | n < 20 = teens !! (n `mod` 10)
>   | n < 100 = let (t,o) = n `divMod` 10 in (tens !! t) ++ " " ++ (numwords o)
>   | n < 1000 = let (h,t) = n `divMod` 100 in
>                 (ones !! h) ++ if t == 0 then " hundred" else " hundred and " ++ (numwords t)
>   | n < 1000000 = let (t,h) = n `divMod` 1000 in (numwords t) ++ " thousand " ++ (numwords h)

Need a thing to count all the letters in this junk.

> count_letters l = length $ filter isLetter l

> euler17 n = count_letters $ intercalate " " $ map numwords [1..n]
