# Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
# $Id: snpp.tcl,v 1.1 2000/06/14 20:01:58 dustin Exp $

set snpp_error -1
set snpp_fd -1
set snpp_hold_state -1
set snpp_msta -1
set snpp_status -1
set snpp_timezone -1
set snpp_twoway_state -1
set snpp_twoway_waiting -1

# SNPP stuff
proc snpp_status_ok { msg } {
	global snpp_status snpp_twoway_state

	set snpp_status [ lindex $msg 0 ]
	set status $snpp_status
	set result -1

	if { $status < 300 && $status >= 200 } {
		set result 0
	}

	# if we're in two-way mode, there are other OK states
	if { $result == -1 && $snpp_twoway_state == 1 } {
		if { $status < 890 && $status >= 860 } {
			set result 0
		} elseif { $status < 970 && $status >= 960 } {
			set result 0
		}
	}

	return $result
}

proc snpp_cmd { snpp_fd cmd } {
	global snpp_error

	puts $snpp_fd "$cmd"
	set ret 0

	flush $snpp_fd

	set line [gets $snpp_fd]
	set snpp_error $line
	set status [ snpp_status_ok $line ]
	if { $status < 0 } {
		# puts "Error: $line"
		# puts "CMD: $cmd"
		# close $snpp_fd
		set ret -1
	}

	return $ret
}

proc snpp_sendpage { host port id msg } {

	global snpp_error snpp_hold_state snpp_timezone snpp_twoway_state
	global snpp_status snpp_fd snpp_twoway_waiting snpp_msta

	set err [catch {set snpp_fd [socket $host $port]}]

	if { $err } {
		set snpp_error "Unable to connect to SNPP server"
		return -1;
	}

	set line [gets $snpp_fd]
	set status [ snpp_status_ok $line ]
	if { $status < 0 } {
		# puts "Error: $line"
		catch { close $snpp_fd }
		return -1
	}

	if { $snpp_twoway_state == 1 } {
		if { [snpp_cmd $snpp_fd "2way"] < 0 } {
			catch { close $snpp_fd }
			return -1
		}
	}

	if { [snpp_cmd $snpp_fd "page $id"] < 0 } {
		catch { close $snpp_fd }
		return -1
	}

	if { [snpp_cmd $snpp_fd "mess $msg"] < 0 } {
		catch { close $snpp_fd }
		return -1
	}

	if { [snpp_cmd $snpp_fd "priority high"] < 0 } {
		# This is actually OK if the error was 500
		if { $snpp_status != 500 } {
			catch { close $snpp_fd }
			return -1
		}
	}

	if { $snpp_hold_state == 1 } {
		set windows {.hold.mdy.m .hold.mdy.d .hold.hms.h
			.hold.hms.m .hold.hms.s}

		# This one is treated a little special...
		set t [ .hold.mdy.y get ]

		foreach window $windows {
			set s [ $window get ]
			# Format for 2 characters if it's not already
			if { [string length $s] < 2 } {
				set s [format "%02d" $s]
			}
			append t $s
		}

		if { [snpp_cmd $snpp_fd "holduntil $t $snpp_timezone"] < 0 } {
			catch { close $snpp_fd }
			return -1
		}
	}

	if { [snpp_cmd $snpp_fd "send"] < 0 } {
		catch { close $snpp_fd }
		return -1
	}

	if { $snpp_twoway_state == 1 } {
		set snpp_msta "[lindex $snpp_error 1] [lindex $snpp_error 2]"
		setstatus "Waiting for response..."

		set snpp_twoway_waiting 1

		update
		after 5000
		while { [check_for_message] < 0 } {
			update
			after 5000
		}
	}

	if { [snpp_cmd $snpp_fd "quit"] < 0 } {
		catch { close $snpp_fd }
		return -1
	}

	catch { close $snpp_fd }

	return 0
}

proc check_for_message { } {
	global snpp_msta snpp_twoway_waiting snpp_fd snpp_twoway_state
	global snpp_status snpp_error

	set r -1

	if { $snpp_twoway_state == 0 } {
		return -1
	}

	if { $snpp_twoway_waiting == 0 } {
		return -1
	}

	if { [snpp_cmd $snpp_fd "msta $snpp_msta"] < 0 } {
		catch { close $snpp_fd }
		return -1
	}

	set response [lrange $snpp_error 4 end]
	if { $snpp_status == 889 } {
		set snpp_twoway_waiting 0
		show_response $response
		set r 0
	} else {
		setstatus $response
	}

	return $r
}

# end SNPP stuff...
