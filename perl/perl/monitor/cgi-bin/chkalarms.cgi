#!/usr/local/bin/perl
# $Id: chkalarms.cgi,v 1.2 1997/12/14 21:31:38 dustin Exp $

push(@INC, "/home/monitor/lib");
require 'statlib.pl';

print <<EOF;
Content-type: text/html

EOF

if(&checkCutoff)
{
    @stats=stat($CUTOFFLOG);
    $time=localtime($stats[9]+$cutoffTimeout);
    print "Alarms are currently quieted.<br>\n";
    print "They will be enabled again $time\n";
}
else
{
    print "Click <a href=\"/~dustin/stats/quiet.shtml\">here</a> to\n";
    print "quiet the alarms temporarily.";
}
