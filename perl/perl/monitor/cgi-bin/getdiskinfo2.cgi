#!/usr/local/bin/perl
# $Id: getdiskinfo2.cgi,v 1.2 1997/12/14 21:31:42 dustin Exp $

push(@INC, "/home/monitor/lib");
require 'statlib.pl';

sub doMB
{
    my($used, $free)=@_;

    print "<table>\n";
    printf "<tr><td><b>Used:</b></td><td>%.2fMB</td></tr>\n", $used/1024;
    printf "<tr><td><b>Free:</b></td><td>%.2fMB</td></tr>\n", $free/1024;
    print "</table>\n";
}

sub getdiskinfo
{
    local($host)=@_;
    local($n, @thing, $have, $havenot);
    @thing=("", "k", "k", "k", "%");

    $SIG{'ALRM'}= 'timeout';
    &openhost($host, 6013);

    print S "disks\r\n";

    chop($n=<S>);

print <<EOF;
<h1>Disks for $host</h1>
EOF

    for(1..$n)
    {
        chop($_=<S>);
        @a=split(/;/);

	$have=100-$a[4];
	$havenot=$a[4];

        print "<table border=\"3\"><tr>";
	print "<td>";
	&doMB($a[2], $a[3]);
	print "</td>";

	print "<td><img src=\"/cgi-bin/stats/makeapie.cgi?$a[0]";
        print "+$havenot%25%20Used=$havenot";
	print "+$have%25%20Free=$have\"></td>\n";
	print "</tr></table>";
    }

    print S "quit\r\n";
    close(S);
}

print <<EOF;
Content-type: text/html

<html><head><title>Disk Usage</title></head>
<body bgcolor="FfFfFf">

EOF

{
    chop;
    &getdiskinfo($ENV{QUERY_STRING});
}

print "<hr width=\"50%\" align=\"left\"><pre>\n";
print '$Id: getdiskinfo2.cgi,v 1.2 1997/12/14 21:31:42 dustin Exp $';
print "\n</pre>\n</body></html>";

