#!/usr/local/bin/perl
# $Id: scanlogs.cgi,v 1.2 1997/12/14 21:31:50 dustin Exp $

push(@INC, "/home/monitor/lib");
require "statlib.pl";

@logFiles=split(/\s+/, $logFiles);
%ht=readInLogstuff();

print <<EOF;
Content-type: text/html

<table border=3 colspan=3>
<tr>
    <td>Log Message</td>
    <td>Occurences</td>
    <td>Last Seen</td>
<tr>
EOF

for $file (@logFiles)
{
    %res=scanLogFile($file);

    for $key (sort(keys(%res)))
    {
        print "<tr><td>$ht{$key}[1]</td><td>$res{$key}[0]</td>";
        print "<td>$res{$key}[1]</td></tr>\n";
    }
}

print <<EOF;
</table>
EOF
