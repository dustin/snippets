We shall say that an n-digit number is pandigital if it makes use of
all the digits 1 to n exactly once; for example, the 5-digit number,
15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 × 186 = 7254,
containing multiplicand, multiplier, and product is 1 through 9
pandigital.

Find the sum of all products whose multiplicand/multiplier/product
identity can be written as a 1 through 9 pandigital.

HINT: Some products can be obtained in more than one way so be sure to
only include it once in your sum.

> import Data.List
> import qualified Data.Set as Set

> digits = "123456789"

The easiest way to implement is_pandigital is to convert the product
to a string of the digits a*b=c and then compare the sorted list to
the list of possible digits.  This is not a particularly efficient way
to do things, but I'm super lazy tonight.

> is_pandigital a b = digits == (sort $ (show a) ++ (show b) ++ (show $ a * b))

Simple/cheap/fast dedup via set.

> dedup :: Ord t => [t] -> [t]
> dedup = Set.toList . Set.fromList

We need to generate a collection of candidates that could at least
theoretically be pandigital.

> gen_nums :: [(Int, Int)]
> gen_nums = dedup $ mc 1 3 ++ mc 1 4 ++ mc 2 3 ++ mc 2 4 ++ mc 3 3
>   where combo a b s = (read $ take a s, read $ (take b . drop a) s)
>         perms = permutations digits
>         mc a b = map (combo a b) perms

Then grab all the pairs that are pandigital.

> all_pans = filter (uncurry is_pandigital) gen_nums

And add up the distinct pandigital products.

> euler32 = sum $ dedup $ map (uncurry (*)) all_pans
