
isPrime :: Integer -> Bool

isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime x
	| even(x)   = False
	| otherwise = rprime(3)
	where rprime(a)
		| (a ^ 2) > x    = True
		| mod x a == 0   = False
		| otherwise      = rprime(a + 2)

