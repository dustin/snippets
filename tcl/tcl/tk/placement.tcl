#!/usr/local/bin/wish8.0
# Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
# $Id: placement.tcl,v 1.2 2000/05/26 02:02:08 dustin Exp $

proc itemStartDrag {c x y} {
	global lastX lastY
	set lastX [.c canvasx $x]
	set lastY [.c canvasy $y]
}

proc itemDrag {c x y} {
	global lastX lastY
	set x [$c canvasx $x]
	set y [$c canvasy $y]
	.c move current [expr $x-$lastX] [expr $y-$lastY]
	set lastX $x
	set lastY $y
}


proc itemDrag {c x y} {
	global lastX lastY
	set x [.c canvasx $x]
	set y [.c canvasy $y]
	.c move current [expr $x-$lastX] [expr $y-$lastY]
	set lastX $x
	set lastY $y
	showAt $x $y
}


proc showAt { x y } {
	global i
	incr i
	set x [ expr int($x) ]
	set y [ expr int($y) ]
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
set background_image [ image create photo -file bg.gif ]
set tigger_image [ image create photo -file tig03.gif ]
.c create image 0 0 -anchor nw -image $background_image
.c create image 1 236 -tags tigger -anchor nw -image $tigger_image


# bind .c <B1-Motion> "showAt %x %y"
bind .c <1> "itemStartDrag .c %x %y"
bind .c <B1-Motion> "itemDrag .c %x %y"

