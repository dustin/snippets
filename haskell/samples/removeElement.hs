-- removeAnElement :: Integer -> [Integer] -> [Integer]
removeAnElement i a = [ x | x <- a , x /= i ]
