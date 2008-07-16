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
		"Starting backup of Fish" application name appName
end tell

do shell script "rsync -rat --delete /Volumes/Fish/ /Users/dustin/bak/Fish/"

tell application "GrowlHelperApp"
	notify with name Â
		"end" title Â
		"Completed" description Â
		"Completed backup of Fish" application name appName
	
end tell