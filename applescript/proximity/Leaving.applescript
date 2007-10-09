-- Turn on the screen saver password
do shell script "defaults -currentHost write com.apple.screensaver askForPassword -int 1"
do shell script "~/local.bin/notif"
-- Activate the screen saver
tell application "ScreenSaverEngine" to activate
