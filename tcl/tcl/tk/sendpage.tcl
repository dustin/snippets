#!/usr/local/bin/wish8.0
# Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
# $Id: sendpage.tcl,v 1.2 1999/08/29 14:02:15 dustin Exp $

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

	set err [catch {set fd [socket $host $port]}]

	if { $err } {
		return -1;
	}

	set line [gets $fd]
	set status [ snpp_status_ok $line ]
	if { $status < 0 } {
		# puts "Error: $line"
		return -1
	}

	if { [snpp_cmd $fd "page $id"] < 0 } {
		return -1
	}

	if { [snpp_cmd $fd "mess $msg"] < 0 } {
		return -1
	}

	if { [snpp_cmd $fd "priority high"] < 0 } {
		return -1
	}

	if { [snpp_cmd $fd "send"] < 0 } {
		return -1
	}

	if { [snpp_cmd $fd "quit"] < 0 } {
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

	set whom [ .whom.whom get ]
	set msg [ .message.what get ]

	if { [ string compare $last_msg $msg ] == 0 &&
		[ string compare $last_uid $whom ] == 0 } {
		setstatus "Page has already been sent."
		return
	}

	setstatus "Sending page..."

	if { [ snpp_sendpage "pager.beyond.com" 1041 $whom $msg ] == 0 } {
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
	set rev { $Revision: 1.2 $ }
	set tmp [ split $rev " " ]
	set version [lindex $tmp 2]
	set msg "Sendpage version $version by Dustin Sallings <dustin@spy.net>"
	set button [tk_messageBox -icon info -type ok \
		-title "About sendpage" -parent . -message $msg ]
}

# START HERE

# Globals, these are needed to ensure someone doesn't accidentally send the
# same page three hundred times.
set last_msg ""
set last_uid ""

wm title . "Page People"
wm iconname . "Pager"

# The width of a text entry thing.
set entwidth 40

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
