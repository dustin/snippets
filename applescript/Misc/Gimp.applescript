tell application "X11" to activate

do shell script "cd ; DISPLAY=:0 /sw/bin/gimp > /dev/null 2>&1 &"
