#!/usr/local/bin/perl
# $Id: quietalarms.cgi,v 1.1 1997/12/12 21:36:01 dustin Exp $

push(@INC, "/home/monitor/lib");
require 'statlib.pl';
require 'cgi-lib.pl';

print <<EOF;
Content-type: text/html

<html><head><title>Alarm Quieter</title></head>
<body bgcolor="fFfFfF">
EOF

&ReadParse(*in);

if(! ($in{'name'}=~/[A-z0-9]/))
{
    print "Sorry, you've gotta at least give your name.\n";
    exit(0);
}

$time=localtime(time());

open(OUT, ">>$CUTOFFLOG");

print OUT <<EOF;
-------------------------------------
Quiet by $in{'name'} at $time

$in{'reason'}
EOF

close(OUT);

print <<EOF;
<h2>Alarm Quieter</h2>

The alarms will be quiet for a while now.

</body></html>
EOF
