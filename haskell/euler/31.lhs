In England the currency is made up of pound, £, and pence, p, and
there are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
It is possible to make £2 in the following way:

1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

How many different ways can £2 be made using any number of coins?


> data Coin = P | P2 | P5 | P10 | P20 | P50 | Pound | Pound2
>           deriving (Eq, Show, Enum, Ord, Bounded)

> value :: Coin -> Int
> value P      = 1
> value P2     = 2
> value P5     = 5
> value P10    = 10
> value P20    = 20
> value P50    = 50
> value Pound  = 100
> value Pound2 = 200

Count the value of the given list of coins.

> count_change :: [Coin] -> Int
> count_change = sum . map value

This one counts coins at quantity.

> count_change' :: [(Int,Coin)] -> Int
> count_change' = sum . map (\(n,c) -> n * (value c))

This is a list of all of the coins in descending order of value.  I
reverse the bounds -- could've just defined them in descending order,
but, eh.

> descoins :: [Coin]
> descoins = reverse [minBound..maxBound]

> make_change :: Int -> [Coin] -> [[(Int,Coin)]]
> make_change _ [] = []
> make_change 0 _ = []
> make_change n coins = case dropWhile (\c -> value c > n) coins of
>                         (coin:coins') -> go (n `div` (value coin)) coin coins'
>                         [] -> []
>   where go 0 c c' = make_change n c'
>         go d c c' = let m = n - (d * (value c)) in
>                       (if m == 0 then [[(d,c)]]
>                        else map ( (d,c) : ) (make_change m c')) ++
>                       (go (pred d) c c')

> euler_31 = length $ make_change 200 descoins
