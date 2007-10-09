tell application "QuickTime Player"
	
	activate
	set this_file to choose file name with prompt "Choose a name and location for the saved frame:" default name ("Crap.pct")
	export front movie to this_file as picture using settings preset "Uncompressed"
	
end tell
