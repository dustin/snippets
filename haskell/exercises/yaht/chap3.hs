import IO

-- Examples
my_length [] = 0
my_length (x:xs) = 1 + my_length xs

my_filter p [] = []
my_filter p (x:xs) =
	if p x
		then x : my_filter p xs
		else my_filter p xs


-- Exercises

-- 3.7 fac
fac 0 = 1
fac 1 = 1
fac n = n * fac (n - 1)

-- 3.8 mult
-- This is a little lame in that it does the comparison every time, but it's
-- alright.
mult _ 0 = 0
mult a 1 = a
mult a b =
	if a > b
	then a + mult a (b - 1)
	else mult b a

-- 3.9 a map
my_map _ [] = []
my_map p (x:xs) =
	(p x) : my_map p xs

-- Another example
askForWords = do
	putStrLn "Please enter a word:"
	word <- getLine
	if word == ""
		then return []
	else do
		rest <- askForWords
		return (word : rest)

-- 3.10 sum input
sum_nums = do
	hSetBuffering stdin LineBuffering
	putStrLn "Give me a bunch of numbers, ending with 0, and I'll add them"
	nums <- sum_nums_helper
	putStrLn ("The sum is " ++ (show (foldr (+) 0 nums)))
	putStrLn ("The product is " ++ (show (foldr (*) 1 nums)))
	show_fac nums

show_fac [] = return ()
show_fac (x:xs) = do
	putStrLn ((show x) ++ "! is " ++ (show (fac x)))
	show_fac xs

sum_nums_helper = do
	putStrLn "Give me a number (0 to finish)"
	line <- getLine
	let v = read line
	if v == 0
		then return []
	else do
		rest <- sum_nums_helper
		return (v : rest)
