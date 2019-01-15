import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Data.List
import Data.Semigroup

data Door = A | B | C
  deriving (Eq, Show, Bounded, Enum)

instance Arbitrary Door where arbitrary = arbitraryBoundedEnum

prop_guess :: Door -> Door -> Positive Int -> Property
prop_guess answer guess (Positive i)
  | answer == guess = collect "switching loses" True
  | answer == changeguess = collect "switching wins" True
  | otherwise = property False
  where changeguess = arb ([A,B,C] \\ remaining)
        remaining = [guess] <> notanswer
        notanswer = [A,B,C] \\ [answer]
        arb l = l !! (i `mod` length l)

{-
+++ OK, passed 10000 tests:
66.61% "switching wins"
33.39% "switching loses"
-}

main :: IO ()
main = quickCheck $ (withMaxSuccess 10000) prop_guess
