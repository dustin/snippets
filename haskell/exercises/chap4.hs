-- Pre-recursion stuff

-- Max of four numbers
maxFour :: Int -> Int -> Int -> Int -> Int
maxFour a b c d = ((a `max` b) `max` c) `max` d

-- weakAscendingOrder is a recomended way to do between
weakAscendingOrder :: Int -> Int -> Int -> Bool
weakAscendingOrder a b c = (b >= a) && (c >= b)

-- true if the second number is between the first and third
between :: Int -> Int -> Int -> Bool
between a b c = weakAscendingOrder a b c || weakAscendingOrder c b a

-- How many of the given list of numbers are equal
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c
	| a == b && b == c  = 3
	| a == b || a == c || b == c = 2
	| otherwise = 0

-- How many of four numbers are equal
-- I just couldn't find a good solution to this without lists or recursion.
-- I feel so lame.
-- howManyOfFourEqual :: Int -> Int -> Int -> Int -> Int

-- 4.5 now allows recursion

rangeProduct :: Int -> Int -> Int
rangeProduct a b
	| b < a		= 0
	| b > a		= rangeProduct a (b - 1) * b
	| a == b	= b

fac :: Int -> Int
fac a = rangeProduct 1 a

addMultiply :: Int -> Int -> Int
addMultiply a b
	| a == 0 || b == 0	= 0
	| a == 1			= b
	| a > 1				= b + addMultiply (a - 1) b

-- Where isn't really supposed to be used here, but it saves some time
isqrt :: Int -> Int
isqrt a = risqrt 1
	where risqrt b
		| b * b > a		= b - 1
		| b < a			= risqrt (b + 1)

f :: Int -> Int
f 0 = 13
f 1 = 44
f 2 = 17
f 3 = 49
f _ = 0

findMaxF :: Int -> Int
findMaxF a
	| a > 0	= max (f a) (findMaxF (a - 1))
	| a == 0	= 0

fContains0 :: Int -> Bool
fContains0 a
	| a < 0		= False
	| f a == 0	= True
	| otherwise	= fContains0 (a - 1)
