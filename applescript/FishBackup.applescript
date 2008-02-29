set the appName to "FishBackup"
tell application "GrowlHelperApp"
	set the allNotificationsList to {"start", "end"}
	set the enabledNotificationsList to {"start", "end"}
	
	register as application Â
		appName all notifications allNotificationsList Â
		default notifications enabledNotificationsList Â
		icon of application "Script Editor"
	
	notify with name Â
		"start" title Â
		"Starting" description Â
		"Starting backup of FISH" application name appName
end tell

do shell script "rsync -rat --delete /Volumes/FISH/ /Users/dustin/bak/FISH/"

tell application "GrowlHelperApp"
	notify with name Â
		"end" title Â
		"Completed" description Â
		"Completed backup of FISH" application name appName
	
end tell