#!/usr/local/bin/perl
# $Id: showxappend.cgi,v 1.2 1997/12/14 21:31:54 dustin Exp $

push(@INC, "/home/monitor/lib");
require 'statlib.pl';

print <<EOF;
Content-type: text/html

<html><head><title>Append data for $ARGV[0]</title></head>
<body bgcolor="#fFfFff">

<h2>Append data for $ARGV[0]</h2>
EOF

if(!open(IN, "$appendBase/$ARGV[0]"))
{
    print "Can't open file $ARGV[0]: $!\n";
    exit;
}

while(<IN>)
{
    chop;
    next if(/^#/);

    print "<p>\n" if(!/[A-z0-9]/);

    print "$_\n";
}

close(IN);

print <<EOF;
</body></html>
EOF
