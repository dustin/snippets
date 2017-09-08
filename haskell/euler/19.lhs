You are given the following information, but you may prefer to
do some research for yourself.

   * 1 Jan 1900 was a Monday.
   * Thirty days has September,
     April, June and November.
     All the rest have thirty-one,
     Saving February alone,
     Which has twenty-eight, rain or shine.
     And on leap years, twenty-nine.
   * A leap year occurs on any year evenly divisible by 4,
     but not on a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth
century (1 Jan 1901 to 31 Dec 2000)?


Let's start with a DoW data type just for fun (starting with Monday,
since that was January 1 1900).

> data DoW = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
>          deriving (Eq, Ord, Enum, Bounded, Show)

Now let's have a list of every day of the week forever.

> days = cycle [minBound..maxBound] :: [DoW]

Same dance for months.

> data Month = January | February | March | April | May | June
>            | July | August | September | October | November | December
>            deriving (Eq, Ord, Enum, Bounded, Show)

> months = [minBound..maxBound] :: [Month]

And years of interest:

> years = [1900..2000]

Probably should have a function that decides if something is a leap year:

> is_leap_year y
>   | y `mod` 400 == 0 = True
>   | y `mod` 100 == 0 = False
>   | y `mod` 4 == 0 = True
>   | otherwise = False

This makes it easy to do a month -> nDays calculator.

> mdays y February = 28 + if is_leap_year y then 1 else 0
> mdays _ m
>       | m `elem` [September, April, June, November] = 30
>       | otherwise = 31

Can I generate a list of possible dates now?

> dates = [ (y, m, d) | y <- years, m <- months, d <- [1..(mdays y m)]]

And here's a list of those dates along with the day of week they fell on:

> dates_dow = zipWith (\(y,m,d) dow -> (y,m,d,dow)) dates days

We can use a simple list comprehension with a pattern match to find
the things we're interested in (Sundays on the first of the month for
years ≥ 1901).

> euler19 = length [x | x@(y,_,d,dow) <- dates_dow, y >= 1901, d == 1, dow == Sunday]
