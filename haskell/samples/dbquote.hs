
-- Quote a string for DB inclusion
dbQuote :: String -> String
dbQuote s = "'" ++ concat(map dbQuoteHelper s) ++ "'"
	where dbQuoteHelper c
		| c == '\'' = "''"
		| otherwise = [c]

