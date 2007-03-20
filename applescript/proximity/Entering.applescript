-- arch-tag: DC77D792-D39C-4DE3-AFA7-4B298CAA3CF2

-- Disable the screen Saver Password
do shell script "defaults -currentHost write com.apple.screensaver askForPassword -int 0"
do shell script "~/local.bin/notif"
-- Turn OFF the screen saver
tell application "ScreenSaverEngine" to quit
-- tell application "Address Book"
-- 	if not unsaved then
-- 		try
-- 			quit
-- 			delay 1
-- 		end try
-- 	end if
-- end tell
-- Reconnect to the Address Book
-- do shell script "defaults write com.apple.AddressBook ABCheckForPhoneNextTime -boolean true"
-- try
-- 	tell application "Address Book"
-- 		launch
-- 	end tell
-- 	tell application "System Events"
-- 		set the visible of process "Address Book" to no
-- 	end tell
-- end try
