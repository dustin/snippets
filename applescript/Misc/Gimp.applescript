-- arch-tag: BCF87BF6-4737-11D8-84C8-000A957659CC
tell application "X11" to activate

do shell script "cd ; DISPLAY=:0 /sw/bin/gimp > /dev/null 2>&1 &"
