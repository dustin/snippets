import Euler
import Test.QuickCheck

digits_symmetry_prop x = x < 0 || (x == (undigits.digits) x)


main :: IO ()
main =
  quickCheck digits_symmetry_prop
