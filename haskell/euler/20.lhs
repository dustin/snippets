n! means n × (n − 1) × ... × 3 × 2 × 1

For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!


Quick hack fact:

> fact x = product [1..x]

...and my digits code from 16

> digits :: Integer -> [Int]
> digits n = map (\c -> read [c]) $ show n

> euler21 = sum $ digits $ fact 100
