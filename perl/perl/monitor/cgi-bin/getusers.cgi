#!/usr/local/bin/perl
# $Id: getusers.cgi,v 1.2 1997/12/14 21:31:47 dustin Exp $

push(@INC, "/home/monitor/lib");
require 'statlib.pl';

sub doidle
{
    local($sec)=@_;
    local($day, $hour, $min, $ret);
    ($day, $hour, $min)=(0, 0, 0);

    if($sec>60)
    {
	$min=int($sec/60);
	$sec=$sec%60;
    }

    if($min>60)
    {
	$hour=int($min/60);
	$min=$min%60;
    }

    if($hour>24)
    {
	$day=int($hour/24);
	$hour=$hour%24;
    }

    if($sec<10)
    {
       $sec="0$sec";
    }

    if($min<10)
    {
       $min="0$min";
    }

    if($hour<10)
    {
       $hour="0$hour";
    }

    if($day>0)
    {
	$ret="$day days, $hour:$min:$sec";
    }
    else
    {
	$ret="$hour:$min:$sec";
    }

    return($ret);
}

sub getusers
{
    local($host)=@_;
    local($n, @thing);
    @thing=("", "k", "k", "k", "%");

    $SIG{'ALRM'}= 'timeout';
    &openhost($host, 6013);

    print S "numusers\r\n";
    chop($n=<S>);
    print S "who\r\n";

print <<EOF;
<h2>$host</h2>
$n users online
EOF

if($n>0)
{
print <<EOF;
<table border="3">
<tr><th>Username</th><th>Idle time</th></tr>
EOF
}

    for(1..$n)
    {
        chop($_=<S>);
	@a=split(/;/, $_);

        print "    <tr>\n";
	print "\t<td><a href=\"mailto:$a[0]\@$mailDomain\">$a[0]</a></td>";
	print  "<td>".&doidle($a[1])."</td>\n";
    }
    if($n>0)
    {
        print "</table>";
    }
    print "<hr width=\"43%\" align=\"left\">\n<p>\n\n";

    print S "quit\r\n";
    close(S);
}

print <<EOF;
Content-type: text/html

EOF

open(IN, $allMachinesFile);
while(<IN>)
{
    chop;
    &getusers($_);
}
close(IN);
