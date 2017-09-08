A unit fraction contains 1 in the numerator. The decimal
representation of the unit fractions with denominators 2 to 10 are
given:

1/2	= 	0.5
1/3	= 	0.(3)
1/4	= 	0.25
1/5	= 	0.2
1/6	= 	0.1(6)
1/7	= 	0.(142857)
1/8	= 	0.125
1/9	= 	0.(1)
1/10	= 	0.1

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It
can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest
recurring cycle in its decimal fraction part.


I wonder if I still know how to divide by hand.

> divf :: (Integral n, Show n) => n -> n -> String
> divf n d = let (x,r) = n `divMod` d in
>   (show x) ++ "." ++ (dodiv r)
>   where dodiv 0 = ""
>         dodiv n = let (x,r) = (10 * n) `divMod` d in
>                     (show x) ++ dodiv r

Well, that's hand division.  Will display all ∞ digits.  I could
identify patterns within it, or I could just lazily consume it looking
for patterns.  The latter is a separation of concerns I like, so I'll
just do that.

I started with a standard floyd algorithm to find the cycles.  In
particular, I grabbed a haskell implementation someone else wrote
since I was being lazy:
https://wiki.haskell.org/Floyd%27s_cycle-finding_algorithm

However, this one failed in various ways, mostly around considering
any digit it happened to see again at the right time a repeating
digit.  It gave bizarre answers for things like 1/11 (for which it
gave ("", "0.") instead of ("0.", "09)).  I've corrected this here by
verifying that the sequence of n at the point of detection must be
equal.

> findCycleN :: Eq a => Int -> [a] -> ([a],[a])
> findCycleN n xxs = fCycle xxs xxs
>   where fCycle l@(x:xs) (_:y:ys)
>          | sub l == sub (y:ys) = fStart xxs xs
>          | otherwise           = fCycle xs ys
>         fCycle _      _        = (xxs,[]) -- not cyclic
>         fStart l@(x:xs) r@(y:ys)
>          | sub l == sub r      = ([], x:fLength l xs)
>          | otherwise           = let (as,bs) = fStart xs ys in (x:as,bs)
>         fLength l r@(y:ys)
>          | sub l == sub r = []
>          | otherwise           = y:fLength l ys
>         sub = take n

The simple findCycle I was looking at doesn't work in many cases.
Rather than list a bunch, I'll describe what I wish one to do as a
property I can throw quickcheck at.  In particular, I should be able
to recreate input by appending the left to the cycled right.  Of
course, quickcheck only found some obvious cases, it wasn't super
great at finding some of the actual ones that came up in solving this
problem.

> fc_reverses :: [Int] -> Bool
> fc_reverses xs = let (l,r) = findCycleN 5 xs in
>                    xs == take (length xs) (l ++ cycle' r)
>   where cycle' [] = [] -- cycle [] doesn't mean anything typically, but it does here.
>         cycle' x = cycle x

A quick function for filtering stuff out while playing around.

> has_cycle n d = case findCycleN 5 $ n `divf` d of
>                   (_, []) -> False
>                   _ -> True

This is a handy thing to format the cycle output to look similar to
what the problem definition has.

> cycleDisplay n d = let (l,r) = findCycleN 5 $ n `divf` d in
>                      if r == "" then l else l ++ "(" ++ r ++ ")"

And for the final answer, we just fold over all the digits, 1-1000 and
pull out the one with the maximum cycle length.

> euler_26 :: Int
> euler_26 = foldr (\n m -> if clen n > clen m then n else m) 1 [1..1000]
>   where clen x = let (_, r) = findCycleN 5 (1 `divf` x) in length r
