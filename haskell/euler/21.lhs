Let d(n) be defined as the sum of proper divisors of n
(numbers less than n which divide evenly into n).

If d(a) = b and d(b) = a, where a ≠ b, then a and b are
an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are
1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110;
therefore d(220) = 284. The proper divisors of 284 are
1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.


> import Euler

This is pretty much d as defined above.

> d = sum.divisors

And this is amicable as defined above.

> amicable a b = a /= b && d a == b && d b == a

So now we just need a list of all of them.  This is going to be a
little wasteful, but it's easiest to just ask if a given number in the
sequence is an amicable number.

> has_amicable :: Integer -> Bool
> has_amicable n = (d n) /= n && n == (d.d) n

> amicables = [x | x <- [1..10000], has_amicable x]

Spit out the answer:

> euler21 = sum amicables

New version with euler lib:
31626
(0.89 secs, 497,064,368 bytes)

31626
[220,284,1184,1210,2620,2924,5020,5564,6232,6368]
(71.60 secs, 36,015,693,848 bytes)
(0.01 secs, 91,424 bytes)


This was kind of slow. I suspect it'd be a lot faster if I generated
the list from the divisors, but now that I have the answer, I kind of
want to move on.
