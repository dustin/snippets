#!/usr/local/bin/perl
# $Id: getdiskinfo.cgi,v 1.2 1997/12/14 21:31:41 dustin Exp $

push(@INC, "/home/monitor/lib");
require 'statlib.pl';

sub getdiskinfo
{
    local($host)=@_;
    local($n, @thing);
    @thing=("", "k", "k", "k", "%");

    $SIG{'ALRM'}= 'timeout';
    &openhost($host, 6013);

    print S "disks\r\n";

    chop($n=<S>);

print <<EOF;
<table border="3">
<tr><th colspan="5"><a href="/cgi-bin/stats/getdiskinfo2.cgi?$host">
$host</a></th></tr>
<tr><td>Mounted on</td><td>Total</td><td>Used</td><td>Free</td><td>% used</td>
</tr>
EOF

    for(1..$n)
    {
        chop($_=<S>);
        @a=split(/;/);
	print "<tr>";
	for(0..$#a)
	{
	    print "<td>$a[$_]$thing[$_]</td>";
	}
	print "</tr>\n";
    }
    print "</table>\n<p>\n\n";

    print S "quit\r\n";
    close(S);
}

print <<EOF;
Content-type: text/html

EOF

open(IN, $diskList);
while(<IN>)
{
    chop;
    &getdiskinfo($_);
}
close(IN);
