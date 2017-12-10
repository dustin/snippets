import Control.Applicative ((<|>))
import Data.List (sort)
import Data.Maybe (fromJust)

checksum :: String -> Int
checksum t = foldr (\x o -> o + mmdiff (map read $ words x)) 0 (lines t)
  where mmdiff l = maximum l - minimum l

checksum' :: String -> Int
checksum' t = foldr (\x o -> o + diven (map read $ words x)) 0 (lines t)
  where diven l = let b = (reverse.sort) l in
          fromJust (findDivs b)

-- Find two divisible numbers from inside a list.
findDivs :: [Int] -> Maybe Int
findDivs [] = Nothing
findDivs (x:xs) = findDivs' x xs <|> findDivs xs
  where findDivs' n [] = Nothing
        findDivs' n (x:xs)
          | n `mod` x == 0 = Just (n `div` x)
          | otherwise = findDivs' n xs
