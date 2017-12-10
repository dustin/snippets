import Data.Char (digitToInt)
import Data.Semigroup ((<>))

-- My original implementations:

-- captcha all@(x:xs) = foldr (\(a,b) o -> o + if a /= b then 0 else digitToInt a) 0 $ zip all (xs <> [x])

-- captcha' xs = foldr (\(a,b) o -> o + if a /= b then 0 else digitToInt a) 0 $ zip xs (drop (length xs `div` 2) $ cycle xs)

-- Refactored for common code.

captchaBy n xs = foldr (\(a,b) o -> o + if a /= b then 0 else digitToInt a) 0 $ zip xs (drop n $ cycle xs)

captcha = captchaBy 1

captcha' xs = captchaBy (length xs `div` 2) xs
