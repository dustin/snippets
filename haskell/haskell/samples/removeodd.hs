-- Remove odd numbers from a list

removeOdd :: [Integer] -> [Integer]
removeOdd a = [ x | x <- a , not(odd(x)) ]
