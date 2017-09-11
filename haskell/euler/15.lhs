Starting in the top left corner of a 2×2 grid, and only being able to move
to the right and down, there are exactly 6 routes to the bottom right corner.


How many such routes are there through a 20×20 grid?

> import Data.List
> import Euler

Function to compute the unique permutations of a given set of things

> np = nub.permutations

Then we generate the number of downs and acrosses we need to do:

> moves d r = (replicate d "↓") ++ (replicate r "→")

Find all unique permutations of those:

> all_moves d r = np $ moves d r

The above doesn't scale very far, though.  An actual number involves math.

> combo n r = (fact (r + n - 1)) `div` ((fact r) * (fact (n - 1)))

To be honest, I'm not super great at math, but I found the plain
combination off by 1/2, so I just doubled it and got a consistent
answer to the above for small numbers.  Seems to get the right answer
at 20x20 as well.

> euler15 = 2 * combo 20 20
