#!/bin/sh
# $Id: getstats.cgi,v 1.2 1997/12/14 21:31:46 dustin Exp $

set kludge { $*
    shift
    shift
    exec /usr/local/bin/scotty -nf $0 $*
}

set stuff {
	cp_user cp_nice cp_system cp_idle dk_xfer_0 dk_xfer_1 dk_xfer_2
	dk_xfer_3 v_pgpgin v_pgpgout v_pswpin v_pswpout v_intr v_swtch
	if_ipackets if_ierrors if_opackets if_oerrors avenrun_0 avenrun_1
	avenrun_2 boottime curtime
}

set mode [expr {$argc == 1 ? [lindex $argv 0] : "html"}]

set stateplot "/cgi-bin/stats/dustin/state.cgi"

proc load {list} {
    set l1m [expr [lindex [lindex $list 18] 2]  / 256.0 ];
    set l5m [expr [lindex [lindex $list 19] 2]  / 256.0 ];
    set l15m [expr [lindex [lindex $list 20] 2] / 256.0 ];

    return "$l1m $l5m $l15m";
}

proc showload {lv} {
    global mode
    global machine
    global stateplot

    if {[string compare $mode "normal"] == 0} {
        puts -nonewline "\tload:  [lindex $lv 0] 1m, "
	puts -nonewline "[lindex $lv 1] 5m, "
	puts "[lindex $lv 2] 15m"
    } elseif {[string compare $mode "log"] == 0} {
	foreach l $lv {
	    puts -nonewline "$l;"
	}
    } elseif {[string compare $mode "html"] == 0} {
	puts "<tr valign=\"top\"><td>\n\t<table border=\"0\">"
	puts -nonewline "\t<tr><td align=\"middle\" colspan=\"2\">"
	    puts "<b>Load averages</b></td></tr>"
	puts "\t<tr><td><a href=\"$stateplot?$machine+load1\">";
	puts "\t1m Load</a></td><td>[lindex $lv 0]</td></tr>"
	puts "\t<tr><td>5m Load</td><td>[lindex $lv 1]</td></tr>"
	puts "\t<tr><td>15m Load</td><td>[lindex $lv 2]</td></tr>"
	puts "\t</table>\n</td>\n"
    }
}

proc diffent { ent1 ent2 who } {
    return [expr [ lindex [lindex $ent2 $who] 2] - \
	[lindex [lindex $ent1 $who ] 2 ]]
}

proc masscalc {list who} {
    set timediff [expr [lindex [lindex $list 22] 2] - \
	[lindex [lindex $list 21 ] 2]]
    return [expr [lindex [lindex $list $who] 2] / $timediff.0]
}

proc cpu { list list2 } {
    set timediff [diffent $list $list2 22];

    set liluser [expr [diffent $list $list2 0] / $timediff.0];
    set lilnice [expr [diffent $list $list2 1] / $timediff.0];
    set lilsys [expr [diffent $list $list2 2] / $timediff.0];
    set lilidle [expr [diffent $list $list2 3] / $timediff.0];

    set biguser [masscalc $list2 0];
    set bignice [masscalc $list2 1];
    set bigsys  [masscalc $list2 2];
    set bigidle [masscalc $list2 3];

    return "$liluser $lilnice $lilsys $lilidle \
	$biguser $bignice $bigsys $bigidle "
}

proc showcpu {vals} {
    global mode
    global machine
    global stateplot
    set fields {"User" "Nice" "Sys" "Idle" "~user" "~nice" "~sys" "~idle"}
    set hf {"User" "Nice" "System" "Idle" }
    set uf {"cp_user" "cp_nice" "cp_system" "cp_idle"
	    "gcp_user" gcp_nice" "gcp_system" "gcp_idle" }
    set nfields 8

    if {[string compare $mode "normal"] == 0} {
        for {set i 0} { $i < $nfields } { incr i} {
            puts "\t[lindex $fields $i]:  [lindex $vals $i]"
        }
    } elseif {[string compare $mode "log"] == 0} {
        foreach val $vals {
            puts -nonewline "$val;"
        }
    } elseif {[string compare $mode "html"] == 0} {
	puts "<td><table border=\"0\">"
	puts "\t<tr><td align=\"middle\" colspan\"2\"><b>CPU</b></td></tr>"

	for { set i 0 } { $i < 4 } { incr i 1 } {
	  puts "\t<tr><td><a"
	  puts "href=\"$stateplot?$machine+[lindex $uf $i]\">"
	  puts "[lindex $hf $i]</a></td>"
	  puts "<td>[lindex $vals $i]</td></tr>"
	}
	puts "\t</table></td>\n"

	puts "<td><table border=\"0\">"

        puts -nonewline "\t<tr><td align=\"middle\" colspan=\"2\">"
	    puts "<b>Global CPU</b></td></tr>"

	for { set i 0 } { $i < 4 } { incr i 1 } {
	   puts "\t<tr><td>[lindex $hf $i]</td>"
	   puts "\t<td>[lindex $vals [expr $i + 4]]</td></tr>"
	}
	puts "</table></td>\n"
    }
}

proc network { list list2 } {
    set timediff [diffent $list $list2 22];

    set inpackets [expr [diffent $list $list2 14] / $timediff.0];
    set outpackets [expr [diffent $list $list2 16] / $timediff.0];

    set gin [masscalc $list2 14]
    set gout [masscalc $list2 16]

    return "$inpackets $outpackets $gin $gout"
}

proc shownetwork {nv} {
    global mode
    global machine
    global stateplot

    if {[string compare $mode "normal"] == 0} {
        puts "\tnet:\n\t\tin: [lindex $nv 0]/s\n\t\tout: [lindex $nv 1]/s"
    } elseif {[string compare $mode "log"] == 0} {
	foreach n $nv {
	    puts -nonewline "$n;"
	}
    } elseif {[string compare $mode "html"] == 0} {
	puts "<td><table align=\"top\" border=\"0\">"
	puts -nonewline "\t<tr><td colspan=\"2\" align=\"middle\">"
	    puts "<b>Network</b></td></tr>"

	puts "\t<tr><td><a href=\"$stateplot?$machine+pack_in\">"
	puts "\tIn</a></td><td>[lindex $nv 0] pkts/s</td></tr>"
	puts "\t<tr><td><a href=\"$stateplot?$machine+pack_out\">"
	puts "\tOut</td><td>[lindex $nv 1] pkts/s</td></tr>"

	puts "</table></td>\n"

	puts "<td><table align=\"top\" border=\"0\">"
	puts -nonewline "\t<tr><td colspan=\"2\" align=\"middle\">"
	    puts "<b>Global Network</b></td></tr>"

	puts "\t<tr><td>In</td><td>[lindex $nv 2] pkts/s</td></tr>"
	puts "\t<tr><td>Out</td><td>[lindex $nv 3] pkts/s</td></tr>"

	puts "</table></td><tr>\n"
    }
}

proc swap { list list2 } {
    set timediff [diffent $list $list2 22];

    set swapin [expr [diffent $list $list2 10] / $timediff.0];
    set swapout [expr [diffent $list $list2 11] / $timediff.0];

    set gswapin [masscalc $list2 10]
    set gswapout [masscalc $list2 11]

    return "$swapin $swapout $gswapin $gswapout"
}

proc showswap { sv } {
    global mode
    global stateplot

    if {[string compare $mode "normal"] == 0} {
	puts "\tSwap (in, out, gin, gout):"
	foreach s $sv {
	    puts "\t\t$s"
	}
    } elseif {[string compare $mode "log"] == 0} {
	foreach s $sv {
	    puts -nonewline "$s;"
	}
    } elseif {[string compare $mode "html"] == 0} {
	puts "<tr valign=\"top\"><td><table border=\"0\">"
	puts -nonewline "\t<tr><td colspan=\"2\" align=\"middle\">"
	    puts "<b>Swap</b></td></tr>"

	puts "<tr><td>Swap In:</td><td>[lindex $sv 0]</td></tr>"
	puts "<tr><td>Swap Out:</td><td>[lindex $sv 1]</td></tr>"

	puts "</table></td>"
	puts "<td><table border=\"0\">"
	puts -nonewline "\t<tr><td colspan=\"2\" align=\"middle\">"
	    puts "<b>Global Swap</b></td></tr>"

	puts "<tr><td>Swap In:</td><td>[lindex $sv 2]</td></tr>"
	puts "<tr><td>Swap Out:</td><td>[lindex $sv 3]</td></tr>"

	puts "</table></td>"
    }
}

proc page { list list2 } {
    set timediff [diffent $list $list2 22];

    set pagein [expr [diffent $list $list2 8] / $timediff.0];
    set pageout [expr [diffent $list $list2 9] / $timediff.0];

    set gpagein [masscalc $list2 8]
    set gpageout [masscalc $list2 9]

    return "$pagein $pageout $gpagein $gpageout"
}

proc showpage { pv } {
    global mode
    global stateplot

    if {[string compare $mode "normal"] == 0} {
	puts "\tPage (in, out, gin, gout):"
	foreach s $pv {
	    puts "\t\t$s"
	}
    } elseif {[string compare $mode "log"] == 0} {
	foreach s $pv {
	    puts -nonewline "$s;"
	}
    } elseif {[string compare $mode "html"] == 0} {
	puts "<td><table border=\"0\">"
	puts -nonewline "\t<tr><td colspan=\"2\" align=\"middle\">"
	    puts "<b>Page</b></td></tr>"

	puts "<tr><td>Page In:</td><td>[lindex $pv 0]</td></tr>"
	puts "<tr><td>Page Out:</td><td>[lindex $pv 1]</td></tr>"

	puts "</table></td>"
	puts "<td><table border=\"0\">"
	puts -nonewline "\t<tr><td colspan=\"2\" align=\"middle\">"
	    puts "<b>Global Page</b></td></tr>"

	puts "<tr><td>Page In:</td><td>[lindex $pv 2]</td></tr>"
	puts "<tr><td>Page Out:</td><td>[lindex $pv 3]</td></tr>"

	puts "</table></td>"
    }
}

proc difftime { t1 t2 } {
    set seconds [ expr $t2 - $t1 ]

    set minutes 0
    set hours 0
    set days 0

    if {$seconds>60} {
	set minutes [expr $seconds / 60 ]
	set seconds [expr $seconds % 60 ]
    }

    if {$minutes>60} {
	set hours [expr $minutes / 60 ]
	set minutes [expr $minutes % 60 ]
    }

    if {$hours>24} {
	set days [expr $hours / 24]
	set hours [expr $hours % 24 ]
    }

    return "$days $hours $minutes $seconds"
}

proc showtop { mc bt ct } {
    global mode
    global stateplot

    set ut [difftime $bt $ct]
    set utd [lindex $ut 0]
    set uth [lindex $ut 1]
    set utm [lindex $ut 2]
    set uts [lindex $ut 3]

    if {[string compare $mode "normal"] == 0} {
	puts -nonewline "$mc"
	puts "  --  up $utd days, $uth hours, $utm minutes and $uts seconds"
    } elseif {[string compare $mode "log"] == 0} {
	puts -nonewline "$mc;[getclock];"
    } elseif {[string compare $mode "html"] == 0} {
	puts "<a name=\"$mc\"></a>"
	puts "<table border=\"3\">"
	puts -nonewline "<tr><th bgcolor=\"efefef\" align=\"middle\""
	    puts "colspan=\"2\">$mc</th>"
	puts -nonewline "<td align=\"left\" colspan=\"3\">"
	    puts -nonewline  "Up $utd days, $uth hours, $utm minutes, "
	    puts "$uts seconds</td></tr>"
    }
}

proc showstats {tm pr} {
    global mode
    global machine
    global stateplot

    if {$tm==1} {
        set s "second"
    } else {
        set s "seconds"
    }

    if {[string compare $mode "normal"] == 0} {
	puts -nonewline  "\tTook $tm $s, $pr ms ping response"
    } elseif {[string compare $mode "log"] == 0} {
	puts -nonewline "$tm;$pr;"
    } elseif {[string compare $mode "html"] == 0} {
	puts -nonewline "<td valign=\"middle\" bgcolor=\"dfdfdf\""
	    puts -nonewline " align=\"middle\">"
	    puts "Took $tm $s<br>to get the info.<br>$pr ms "
	puts "<a href=\"$stateplot?$machine+ping\">ping time</a></td>"
    }
}

proc shownoanswer { mac } {
    global mode

    if {[string compare $mode "normal"] == 0} {
	puts "$mac is unreachable"
    } elseif {[string compare $mode "html"] == 0} {
	puts "<font color=\"ff0000\" size=\"+3\"><b><blink>"
	puts "$mac unreachable</blink></b></font><p>"
    }
}

# main here

if {[string compare $mode "html"] == 0} {
    puts "Content-type: text/html\n"
    set sleeptime 1
} else {
    set sleeptime 5
}

set file "/home/monitor/lib/thelist"

if [catch {open $file} input] {
    puts "<br><b>ERROR: Unable to open thelist</b></br>"
    exit
}

set machines {}

while {[gets $input machine] != -1 } {
    lappend machines $machine
}

foreach machine $machines {
    set pt [lindex [lindex [icmp echo $machine] 0] 1]
    if { $pt>-1 } {
        set clk1($machine) [getclock]
        set list($machine) [ sunrpc stat $machine ]
        set clk2($machine) [getclock]
    } else {
	shownoanswer $machine
    }
}

# sleep so we can check the times
sleep $sleeptime

foreach machine $machines {
    set pt [lindex [lindex [icmp echo $machine] 0] 1]
    if { $pt>-1 } {
        set list2($machine) [ sunrpc stat $machine ]
    } else {
	shownoanswer $machine
    }
}

foreach machine $machines {

        set stats [expr {($clk2($machine)-$clk1($machine)) }]

        showtop $machine [lindex [lindex $list2($machine) 21] 2] \
            [lindex [lindex $list2($machine) 22] 2]

        set loadvals [load $list($machine)];
        showload $loadvals

        set cpuvals [ cpu $list($machine) $list2($machine) ]
        showcpu $cpuvals

        set netvals [network $list($machine) $list2($machine)]
        shownetwork $netvals

        set swapvals [swap $list($machine) $list2($machine)]
        showswap $swapvals

        set pagevals [page $list($machine) $list2($machine)]
        showpage $pagevals

        showstats $stats $pt

        if {[string compare $mode "log"] == 0} {
            puts ""
        } elseif {[string compare $mode "html"] == 0} {
            puts "</tr></table><p>"
        }
}
