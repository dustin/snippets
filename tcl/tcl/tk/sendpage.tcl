#!/usr/local/bin/wish8.0
# Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
# $Id: sendpage.tcl,v 1.7 1999/09/04 06:40:45 dustin Exp $

# SNPP stuff
proc snpp_status_ok { msg } {
	set status [ lindex $msg 0 ]
	set result -1

	if { $status < 300} {
		if { $status >= 200} {
			set result 0
		}
	}

	return $result
}

proc snpp_cmd { fd cmd } {
	global snpp_error

	puts $fd "$cmd"
	set ret 0

	flush $fd

	set line [gets $fd]
	set status [ snpp_status_ok $line ]
	if { $status < 0 } {
		set snpp_error $line
		# puts "Error: $line"
		# puts "CMD: $cmd"
		close $fd
		set ret -1
	}

	return $ret
}

proc snpp_sendpage { host port id msg } {

	global snpp_error hold_state timezone

	set err [catch {set fd [socket $host $port]}]

	if { $err } {
		set snpp_error "Unable to connect to SNPP server"
		return -1;
	}

	set line [gets $fd]
	set status [ snpp_status_ok $line ]
	if { $status < 0 } {
		# puts "Error: $line"
		catch { close $fd }
		return -1
	}

	if { [snpp_cmd $fd "page $id"] < 0 } {
		catch { close $fd }
		return -1
	}

	if { [snpp_cmd $fd "mess $msg"] < 0 } {
		catch { close $fd }
		return -1
	}

	if { [snpp_cmd $fd "priority high"] < 0 } {
		catch { close $fd }
		return -1
	}

	if { $hold_state == 1 } {
		set windows {.hold.mdy.y .hold.mdy.m .hold.mdy.d .hold.hms.h
			.hold.hms.m .hold.hms.s}

		set t ""

		foreach window $windows {
			set s [ $window get ]
			append t $s
		}

		if { [snpp_cmd $fd "holduntil $t $timezone"] < 0 } {
			catch { close $fd }
			return -1
		}
	}

	if { [snpp_cmd $fd "send"] < 0 } {
		catch { close $fd }
		return -1
	}

	if { [snpp_cmd $fd "quit"] < 0 } {
		catch { close $fd }
		return -1
	}

	catch { close $fd }

	return 0
}

# end SNPP stuff...

# Set the status message at the bottom.
proc setstatus { text } {
	.status.msg configure -state normal
	.status.msg delete 0 1000
	.status.msg insert 0 $text
	.status.msg configure -state disabled
}

# Send the actual page as defined in the gui...
proc sendpage { } {
	global snpp_error
	global last_msg last_uid
	global snpp_server snpp_port

	set snpp_error "Unknown error"

	set whom [ .whom.whom get ]
	set msg [ .message.what get ]

	if { [ string compare $last_msg $msg ] == 0 &&
		[ string compare $last_uid $whom ] == 0 } {
		setstatus "Page has already been sent."
		return
	}

	setstatus "Sending page..."

	if { [ snpp_sendpage $snpp_server $snpp_port $whom $msg ] == 0 } {
		setstatus "Page sent succesfully"
		set last_msg $msg
		set last_uid $whom
	} else {
		setstatus "Error sending page:  $snpp_error"
	}
}

# Clear the form fields in the gui, and the stuff that'd keep us from
# paging again.
proc clearstuff { } {
	.whom.whom delete 0 1000
	.message.what delete 0 1000
	set last_msg ""
	set last_uid ""
	setstatus ""
}

# Tell us about yourself...
proc about { } {
	set rev { $Revision: 1.7 $ }
	set tmp [ split $rev " " ]
	set version [lindex $tmp 2]
	set msg "Sendpage version $version by Dustin Sallings <dustin@spy.net>"
	set button [tk_messageBox -icon info -type ok \
		-title "About sendpage" -parent . -message $msg ]
}

# Preferences store

proc preferences_store { p } {
	global snpp_server snpp_port timezone

	set snpp_server [ $p.server.server get ]
	set snpp_port [ $p.port.port get ]
	set timezone [ $p.timezone.timezone get ]
	write_config
}

# Preferences window.

proc preferences { } {
	global snpp_server snpp_port timezone

	set p .preferences
	catch { $p destroy }
	toplevel $p

	# The server field.
	frame $p.server
	label $p.server.msg -text "Server"
	entry $p.server.server
	$p.server.server insert 0 $snpp_server
	pack $p.server.msg -side left -expand 1
	pack $p.server.server -side right -expand 1
	pack $p.server -side top -expand 1 -fill x

	# The port field.
	frame $p.port
	label $p.port.msg -text "Port"
	entry $p.port.port
	$p.port.port insert 0 $snpp_port
	pack $p.port.msg -side left -expand 1
	pack $p.port.port -side right -expand 1
	pack $p.port -side top -expand 1 -fill x

	# The timezone field.
	frame $p.timezone
	label $p.timezone.msg -text "Timezone"
	entry $p.timezone.timezone
	$p.timezone.timezone insert 0 $timezone
	pack $p.timezone.msg -side left -expand 1
	pack $p.timezone.timezone -side right -expand 1
	pack $p.timezone -side top -expand 1 -fill x

	# The buttons.
	frame $p.buttons
	button $p.buttons.save -text "Save" -command "preferences_store $p"
	button $p.buttons.done -text "Done" -command "destroy $p"
	pack $p.buttons.save $p.buttons.done -side left -expand 1
	pack $p.buttons -side top -expand 1 -fill x
}

proc toggle_hold { } {
	global hold_state

	set windows {.hold.mdy.m .hold.mdy.d .hold.mdy.y .hold.hms.h
		.hold.hms.m .hold.hms.s}

	# Make sure they're all configured modifyable
	foreach window $windows {
		$window configure -state normal
	}

	# Delete all the current data.
	foreach window $windows {
		$window delete 0 1000
	}

	if { $hold_state == 0 } {
		# Enable the hold state
		set hold_state 1

		# Put the current time in the holduntil box
		set time [ clock format [clock seconds] -format "%m %d %Y %H %M %S" ]
		.hold.mdy.m insert 0 [lindex $time 0]
		.hold.mdy.d insert 0 [lindex $time 1]
		.hold.mdy.y insert 0 [lindex $time 2]
		.hold.hms.h insert 0 [lindex $time 3]
		.hold.hms.m insert 0 [lindex $time 4]
		.hold.hms.s insert 0 [lindex $time 5]
	} else {
		# Disable the hold state
		set hold_state 0

		foreach window $windows {
			$window configure -state disabled
		}
	}
}

proc read_config {} {
	global snpp_server snpp_port timezone

	set err [ catch { set fd [ open "sendpage.cnf" r ] } ]

	if { $err } {
		return
	}

	while { [ gets $fd line ] != -1 } {
		eval $line
	}

	catch { [ close $fd ] }
}

# Save the running config to disk.
proc write_config {} {
	global snpp_server snpp_port timezone

	set err [ catch { set fd [ open "sendpage.cnf" w ] } ]

	if { $err } {
		return
	}

	# write out all necessary variables.
	puts $fd "# DO NOT EDIT THIS!"

	foreach var {snpp_server snpp_port timezone} {
		set value ""
		eval append value $$var
		puts $fd "set $var $value"
	}

	catch { [ close $fd ] }
}

# START HERE

# Globals, these are needed to ensure someone doesn't accidentally send the
# same page three hundred times.
set last_msg ""
set last_uid ""

# Defaults
set snpp_server "pager.beyond.com"
set snpp_port   1041
set timezone -8

read_config

set hold_state 1

wm title . "Page People"
wm iconname . "Pager"

# The width of a text entry thing.
set entwidth 40

# Menus
set m .menu
menu $m -tearoff 1
menu $m.file -tearoff 1
$m add cascade -label "File" -menu $m.file -underline 0
$m.file add command -label "Preferences" -command "preferences"
$m.file add command -label "Quit" -command "exit"

. configure -menu $m

# The Whom field.
frame .whom
label .whom.msg -text "Whom"
entry .whom.whom -width $entwidth
pack .whom.msg -side left -expand 1
pack .whom.whom -side right -expand 1
pack .whom -side top -expand 1 -fill x

# The What field.
frame .message
label .message.msg -text "What"
entry .message.what -width $entwidth
pack .message.msg -side left -expand 1
pack .message.what -side right -expand 1
pack .message -side top -expand 1 -fill x

# The HOLDuntil thingy
frame .hold
label .hold.l -text "Hold"
checkbutton .hold.hold -command toggle_hold

pack .hold.l .hold.hold -side left -expand 1

frame .hold.mdy
label .hold.mdy.mdy -text "m/d/y"
entry .hold.mdy.m -width 2
entry .hold.mdy.d -width 2
entry .hold.mdy.y -width 4

pack .hold.mdy .hold.mdy.mdy .hold.mdy.m .hold.mdy.d .hold.mdy.y \
	-side left -expand 1

frame .hold.hms
label .hold.hms.hms -text "h:m:s"
entry .hold.hms.h -width 2
entry .hold.hms.m -width 2
entry .hold.hms.s -width 2

pack .hold.hms .hold.hms.hms .hold.hms.h .hold.hms.m .hold.hms.s \
	-side left -expand 1

pack .hold -side top -expand 1 -fill x

# Set the hold state, initialize crap.
toggle_hold

# The buttons.
frame .buttons
button .buttons.page -text "Send" -command "sendpage"
button .buttons.clear -text "Clear" -command "clearstuff"
button .buttons.quit -text "Quit" -command "exit"
button .buttons.about -text "About" -command "about"
pack .buttons.page .buttons.clear .buttons.quit .buttons.about \
	-side left -expand 1
pack .buttons -side top -expand 1 -fill x

# The status bar.
frame .status
label .status.l -text "Status: "
entry .status.msg -width $entwidth -relief flat -state disabled
pack .status.l -side left -expand 1
pack .status.msg -side right -expand 1
pack .status -side bottom -expand 1 -fill x
