module Main
	where

import IO
import Random

main = do
	hSetBuffering stdin LineBuffering
	num <- randomRIO (1::Int, 100)
 	putStrLn "I have chosen a number between 1 and 100"
	doGuessing num

doGuessing num = do
	putStrLn "Guess:"
	guess <- getLine
	let guessNum = read guess
	if guessNum < num
		then do putStrLn "Too low!"
			doGuessing num
		else if guessNum > num
			then do putStrLn "Too high!"
				doGuessing num
			else do putStrLn "Yep."
