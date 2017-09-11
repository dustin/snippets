-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the number 600851475143 ?

import Euler

largest_factor n = last (factor' n)

euler3 = largest_factor 600851475143
