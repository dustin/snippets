-- removeAnElement :: Integer -> [Integer] -> [Integer]
removeAnElement i a = [ x | x <- a , not(x == i) ]
