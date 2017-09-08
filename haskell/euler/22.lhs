Using names.txt (right click and 'Save Link/Target As...'), a 46K text
file containing over five-thousand first names, begin by sorting it
into alphabetical order. Then working out the alphabetical value for
each name, multiply this value by its alphabetical position in the
list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN,
which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the
list. So, COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?


> import qualified Data.ByteString.Lazy.Char8 as BL
> import Data.List (sort)

Score a single name (e.g. COLIN -> 53)

> score_name :: String -> Int
> score_name = foldr (\l s -> (1 + fromEnum l - fromEnum 'A') + s) 0

Scoring all involves multiplying the score of the name by its position
in the sorted name list.

> score_all :: [String] -> Int
> score_all l = foldr (\(n,pos) s -> pos * (score_name n) + s) 0 $ zip (sort l) [1..]

Parse here is just a convenient wrapper for read that declares the
input and output types to avoid confusion.  It could be inferred, but
this let me play interactively a bit more.

> parse :: String -> [String]
> parse = read

This is a bit more complicated than way I have been doing it because I
wanted to isolate the IO, so euler_23 is an IO () function that allows
the rest to be pure.

> euler_23 = do
>   d <- BL.readFile "p022_names.txt"
>   print $ score_all $ parse $ "[" ++ (BL.unpack d) ++ "]"
