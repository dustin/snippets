
isPrime :: Integer -> Bool

isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime x
	| even(x)   = False
	| otherwise = rprime 3
	where rprime a
		| (a ^ 2) > x    = True
		| mod x a == 0   = False
		| otherwise      = rprime (a + 2)

-- The above implementation was used to create the list of the first 100
-- primes for bootstrapping this list the following way:
-- take 100 [ x | x <- [0..], isPrime(x)]
primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,
			89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,
			179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,
			269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,
			367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,
			461,463,467,479,487,491,499,503,509,521,523]
				++ [ x | x <- [541..], isPrime2(x) ]

-- A version of isPrime that uses the above list.
isPrime2 :: Integer -> Bool

isPrime2 0 = False
isPrime2 1 = False
isPrime2 a = rprime(primes)
	where rprime(x:xs)
		| x == a       = True        -- Matching prime
		| (x ^ 2) > a  = True        -- Only do up to the square root
		| mod a x == 0 = False       -- composite if divisible
		| otherwise    = rprime(xs)  -- Recurse

main = putStr(show ([ x | x <- [0..], isPrime2(x)] !! 100000) ++ "\n")
