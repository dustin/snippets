import Euler
import Test.QuickCheck
import Test.QuickCheck.Modifiers

digits_symmetry_prop (Positive x) = x == (undigits.digits) x

divisors_divise_prop (Positive x) = all (\d -> x `mod` d == 0) $ divisors x

factor_divise_prop (Positive x) = all (\d -> x `mod` d == 0) $ factor x

factor'_divise_prop (Positive x) = all (\d -> x `mod` d == 0) $ factor' x

main :: IO ()
main = do
  quickCheck digits_symmetry_prop
  quickCheck divisors_divise_prop
  quickCheck factor_divise_prop
  quickCheck factor'_divise_prop

