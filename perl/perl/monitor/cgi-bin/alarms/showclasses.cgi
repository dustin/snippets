#!/usr/local/bin/perl

push(@INC, "/home/monitor/lib");
require 'statlib.pl';

print <<EOF;
Content-type: text/html

<html><head><title>Alarm Classes</title></head>
<body bgcolor="fFfFfF">

<h2>Alarm Entries</h2>

EOF

@hs=readInAlarms();

foreach $key (sort(keys(%{$hs[0]})))
{
    print "<li>\n\t$key\n\t\t<ul>";
    foreach $line (sort(@{ $hs[0]{$key} }))
    {
        print "\t\t\t<li>$line</li>\n";
    }
    print "\t\t</ul>\t</li>\n";
}

print "</ul>\n";

print "<h2>Alarm Classes</h2>\n";

foreach $key (sort(keys(%{$hs[1]})))
{
    print "<li>\n\t$key\n\t\t<ul>";
    foreach $line (sort(@{ $hs[1]{$key} }))
    {
        print "\t\t\t<li>$line</li>\n";
    }
    print "\t\t</ul>\t</li>\n";
}

print <<EOF;
</ul>
</body>
</html>
EOF
