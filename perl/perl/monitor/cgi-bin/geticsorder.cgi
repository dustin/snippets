#!/usr/local/bin/perl
# $Id: geticsorder.cgi,v 1.2 1997/12/14 21:31:42 dustin Exp $

push(@INC, "/home/monitor/lib");
require 'statlib.pl';

@hosts=("melchior.ic3.com");
@vars=("ICS Orders");

%am=&appmonInit;

print <<EOF;
Content-type: text/html

ICS Orders:
EOF

$color="007f00";
$status=0;

foreach $var (sort(@vars))
{
    foreach $host (sort(@hosts))
    {
	if(defined($am{$host}{$var}))
	{
	    if($am{$host}{$var}=~/fail/i)
	    {
                $status=1;
		$color="ff0000";
	    }
	}
    }
}

print "<font color=\"$color\">";

if($status==1)
{
    print "Failed.";
}
else
{
    print "Passed.";
}

print "</font>\n";
