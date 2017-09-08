By starting at the top of the triangle below and moving to
adjacent numbers on the row below, the maximum total from top to bottom is 23.

         3
        7 4
       2 4 6
      8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:

                     75
                    95 64
                   17 47 82
                  18 35 87 10
                 20 04 82 47 65
                19 01 23 75 03 34
               88 02 77 73 07 63 67
              99 65 04 28 06 16 70 92
             41 41 26 56 83 40 80 70 33
            41 48 72 33 47 32 37 16 94 29
           53 71 44 65 25 43 91 52 97 51 14
          70 11 33 28 77 73 17 78 39 68 17 57
         91 71 52 38 17 14 91 43 58 50 27 29 48
        63 66 04 68 89 53 67 30 73 16 69 87 40 31
       04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

NOTE: As there are only 16384 routes, it is possible to solve this
problem by trying every route. However, Problem 67, is the same
challenge with a triangle containing one-hundred rows; it cannot be
solved by brute force, and requires a clever method! ;o)



> import Control.Concurrent.Async

The first hard thing here is figuring out how to represent this data
in memory.  The obvious thing is a DAG since it's a sort of tree where
most items have multiple parents.

> data DAG = Leaf Int
>          | Node Int (DAG, DAG)
>          | Blank deriving(Show, Eq)

Let's see if we can construct a DAG with our definition.  A hand-coded
version of the small test above makes it easy to verify our build does
something sensible.

> testd = Node 3 (
>             Node 7 (
>                 Node 2 (Leaf 8, Leaf 5),
>                 Node 4 (Leaf 5, Leaf 9)),
>             Node 4 (
>                 Node 4 (Leaf 5, Leaf 9),
>                 Node 6 (Leaf 9, Leaf 3)))

Looking at the triangle all centered is intimidating.  Left justified
is closer to how I'm going to parse it.

3
7 4
2 4 6
8 5 9 3

> testInput :: [[Int]]
> testInput = [[3],
>              [7, 4],
>              [2, 4, 6],
>              [8, 5, 9, 3]]

Here's the descent parser:

> parseTriangle :: [[Int]] -> DAG
> parseTriangle [] = Blank
> parseTriangle (x:[]) = Leaf $ head x
> parseTriangle (x:xs) = Node (head x) (parseTriangle xs, parseTriangle $ map tail xs)


Let's do the real one now:

> realInput = parseTriangle [[75],
>                            [95, 64],
>                            [17, 47, 82],
>                            [18, 35, 87, 10],
>                            [20, 04, 82, 47, 65],
>                            [19, 01, 23, 75, 03, 34],
>                            [88, 02, 77, 73, 07, 63, 67],
>                            [99, 65, 04, 28, 06, 16, 70, 92],
>                            [41, 41, 26, 56, 83, 40, 80, 70, 33],
>                            [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
>                            [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
>                            [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
>                            [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
>                            [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
>                            [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]]

Heres a simple brute force implementation of summing:

> maxsum :: DAG -> Int
> maxsum Blank = 0
> maxsum (Leaf x) = x
> maxsum (Node x (l, r)) = x + (max (maxsum l) (maxsum r))

Let's go ahead and do a concurrent one to brute force faster.  All
stuff that happens in other threads happens in the IO monad, which is
mildly annoying, but we can deal with it.

> maxsum' :: Int -> DAG -> IO Int
> maxsum' _ Blank = return 0
> maxsum' 0 (Leaf x) = return x
> maxsum' 0 (Node x (l, r)) = do
>   ls <- maxsum' 0 l
>   rs <- maxsum' 0 r
>   return $ x + (max ls rs)
> maxsum' t (Node x (l, r)) = do
>     (ls, rs) <- concurrently (maxsum' (pred t) l) (maxsum' (pred t) r)
>     return $ x + (max ls rs)

Turns out, maxsum' is slower than maxsum in my small tests.  I didn't
have an opportunity to do a large criterion test because of
dependencies I don't want to deal with at the moment, but I get an
answer in .01s with maxsum, so I'm not going to complain.
