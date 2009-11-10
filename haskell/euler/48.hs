-- The series, 1^(1) + 2^(2) + 3^(3) + ... + 10^(10) = 10405071317.

-- Find the last ten digits of the series, 1^(1) + 2^(2) + 3^(3) + ... + 1000^(1000).

sumOfPowers :: Int -> Integer
sumOfPowers m = sum $ take m [n^n | n <- [1..]]

euler48 n = reverse $ take 10 $ reverse $ show $ sumOfPowers n
