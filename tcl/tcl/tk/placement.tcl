#!/usr/local/bin/wish8.0
# Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
# $Id: placement.tcl,v 1.1 2000/05/25 08:11:38 dustin Exp $

proc showAt { x y } {
	global i
	incr i
	if { $i % 5 == 0 } {
		puts "combine -compose over -geometry +$x+$y bg.gif tig03.gif /tmp/anim/$i.png"
		flush stdout
	}
}

set width [lindex $argv 0]
set height [lindex $argv 1]

set i 0

wm title . "Placement"
wm iconname . "Placement"

canvas .c -relief raised -width $width -height $height
pack .c -side top -fill x

bind .c <B1-Motion> "showAt %x %y"

