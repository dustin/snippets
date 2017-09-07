2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?


This is kind of dumb, but hacky way to turn a number into a sequence of digits.

> digits :: Integer -> [Int]
> digits n = map (\c -> read [c]) $ show n

> euler16 = sum $ digits $ 2 ^ 1000
