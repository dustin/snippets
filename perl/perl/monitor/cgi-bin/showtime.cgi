#!/usr/bin/perl
# $Id: showtime.cgi,v 1.1 1997/12/12 21:36:01 dustin Exp $

push(@INC, "/home/monitor/lib");
require 'statlib.pl';

sub getusers
{
    local($host)=@_;
    local($n, @thing);

    $SIG{'ALRM'}= 'timeout';
    &openhost($host, 6013);

    print S "localtime\r\n";
    $n=<S>;
    print "<tr><td>$host</td><td>$n</td></tr>\n";
    print S "quit\r\n";
    close(S);
}

print <<EOF;
Content-type: text/html

<table border="3">
<tr><td>Host</td><td>Time</td></tr>
EOF

open(IN, $allMachinesFile);
while(<IN>)
{
    chop;
    &getusers($_);
}
close(IN);

print <<EOF;
</table>
EOF
