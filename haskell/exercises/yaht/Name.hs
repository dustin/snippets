module Main
	where

import IO

main = do
	hSetBuffering stdin LineBuffering
	putStrLn "Please enter your name:  "
	name <- getLine
	putStrLn ("hello, " ++ name ++ ", how are you?")
