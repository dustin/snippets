-- The sum of the squares of the first ten natural numbers is,
-- 1^(2) + 2^(2) + ... + 10^(2) = 385

-- The square of the sum of the first ten natural numbers is,
-- (1 + 2 + ... + 10)^(2) = 55^(2) = 3025

-- Hence the difference between the sum of the squares of the first
-- ten natural numbers and the square of the sum is 3025 − 385 = 2640.

-- Find the difference between the sum of the squares of the first one
-- hundred natural numbers and the square of the sum.

sum_of_squares :: [Integer] -> Integer

sum_of_squares l = sum [ n * n | n <- l ]

square_of_sums :: [Integer] -> Integer

square_of_sums l = (sum l) ^ 2

euler_6 :: [Integer] -> Integer

euler_6 l = square_of_sums l - sum_of_squares l