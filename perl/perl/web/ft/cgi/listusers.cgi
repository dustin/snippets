#!/usr/local/bin/perl

push(@INC, "/home/dustins/lib");
require 'filething.pl';

print <<EOF;
Content-type: text/html

EOF

for $key (sort(keys(%userMap)))
{
    print "<option value=\"$key\">$userMap{$key}\n";
}
