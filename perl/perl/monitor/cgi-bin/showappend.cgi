#!/usr/local/bin/perl
# $Id: showappend.cgi,v 1.2 1997/12/14 21:31:51 dustin Exp $

push(@INC, "/home/monitor/lib");
require 'statlib.pl';

sub getftoday
{
    local($mon, $day, $year)=@_;

    $mon="0$mon" if($mon<10);
    $day="0$day" if($day<10);
    $year="0$year" if($year<10);

    return("$mon$day$year");
}

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)=localtime(time());

$mon++;

$ftoday=&getftoday($mon, $mday, $year);

print "Content-type: text/html\n\n";

if(!open(IN, "$appendBase/$ftoday"))
{
    print "No Messages for today.";
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
