-- A palindromic number reads the same both ways. The largest palindrome
-- made from the product of two 2-digit numbers is 9009 = 91 × 99.

-- Find the largest palindrome made from the product of two 3-digit numbers.

pairs :: Integer -> Integer -> [(Integer, Integer)]

pairs low high = [ (x, y) | x <- [low..high], y <- [x..high]]

palindromes :: [(Integer, Integer)] -> [Integer]

palindromes from = [ x * y | (x,y) <- from,
            show (x * y) == reverse (show (x * y))]

biggest_palindrome :: Integer -> Integer -> Integer

biggest_palindrome low high = maximum (palindromes (pairs low high))
