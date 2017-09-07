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


> numwords 0 = ""
> numwords n
>   | n < 10 = ones !! n
>   | n < 20 = teens !! (n `mod` 10)
>   | n < 100 = let (t,o) = n `divMod` 10 in (tens !! t) ++ " " ++ (ones !! o)
>   | n < 1000 = let (h,t) = n `divMod` 100 in
>                 (ones !! h) ++ if t == 0 then " hundred" else " hundred and " ++ (numwords t)
>   | n == 1000 = "one thousand"

Need a thing to count all the letters in this junk.

> count_letters l = length $ filter isLetter l

> euler17 n = count_letters $ intercalate " " $ map numwords [1..n]
