Starting with the number 1 and moving to the right in a clockwise
direction a 5 by 5 spiral is formed as follows:

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

It can be verified that the sum of the numbers on the diagonals is 101.

What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?


Starting with thinking a bit...  1x1 = 1.   3x3 is 25.  5x5 is 101  7x7 = 261

Pattern's a bit weird, but it's pretty clear that the progression up
and right is n^2 for n rings.  That means we know the upper right
corner, and the other corners are just that - n - 1.

43 44 45 46 47 48 49
41 21 22 23 24 25 26
41 20  7  8  9 10 27
40 19  6  1  2 11 29
39 18  5  4  3 12 29
38 17 16 15 14 13 30
37 36 35 34 33 32 31


> count_ring :: Int -> Int
> count_ring 1 = 1
> count_ring n
>   | odd n = count_ring (n - 2) + sum [n^2 - (n - 1) * x | x <- [0..3]]


> euler_28 = count_ring 1001
