#!/usr/local/bin/perl
# $Id: getqsdata.cgi,v 1.2 1997/12/14 21:31:45 dustin Exp $

push(@INC, "/home/monitor/lib");
require 'statlib.pl';

sub getqs
{
    local($host)=@_;
    local(%h, @a);

print <<EOF;
<h2>$host</h2>
EOF

    %h=%{$appHash{$host}};

    @a=sort(keys(%h));

if($#a>=0)
{
print <<EOF;
<table border="3">
<tr><th>Key</th><th>Data</th></tr>
EOF
}

    foreach(@a)
    {
        print "<tr><td>$_</td><td>$h{$_}</td></tr>\n";
    }
    if($#a>=0)
    {
        print "</table>";
    }
    print "<hr width=\"43%\" align=\"left\">\n<p>\n\n";
}

print <<EOF;
Content-type: text/html

EOF

%appHash=&appmonInit;

foreach (sort(keys(%appHash)))
{
    &getqs($_);
}
