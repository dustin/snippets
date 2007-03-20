-- arch-tag: 226A9E73-246E-46C6-A2A3-808BB9468927

-- Turn on the screen saver password
do shell script "defaults -currentHost write com.apple.screensaver askForPassword -int 1"
do shell script "~/local.bin/notif"
-- Activate the screen saver
tell application "ScreenSaverEngine" to activate
