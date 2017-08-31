fizzbuzz x
  | m3 && m5 = "fizzbuzz"
  | m3 = "fizz"
  | m5 = "buzz"
  | otherwise = ""
  where m3 = x `mod` 3 == 0
        m5 = x `mod` 5 == 0

fizzbuzzseq = [(x, fizzbuzz x) | x <- [1..]]

main = putStrLn $ show $ take 100 fizzbuzzseq
