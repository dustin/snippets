#!/usr/bin/perl
# $Id: getdiskinfo.cgi,v 1.3 1997/12/31 19:37:49 dustin Exp $

use CGI;
use Collapse;

push(@INC, "/home/monitor/lib");
require 'statlib.pl';

sub getdiskinfo
{
    my($host)=@_;
    my($n, @thing, $tab);
    @thing=("", "k", "k", "k", "%");

    $SIG{'ALRM'}= 'timeout';
    &openhost($host, 6013);

    print S "disks\r\n";

    chop($n=<S>);

$tab=<<EOF;
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
	$tab.="<tr>";
	for(0..$#a)
	{
	    $tab.="<td>$a[$_]$thing[$_]</td>";
	}
	$tab.="</tr>\n";
    }
    $tab.="</table>\n<p>\n\n";

    print S "quit\r\n";
    close(S);
    return($tab);
}

print <<EOF;
Content-type: text/html

EOF

push(@stuff, "<font size=\"+2\"><b>Disk list</b></font>");

open(IN, $diskList);
while(<IN>)
{
    chop;
    push(@stuff, [$_, &getdiskinfo($_)]);
}
close(IN);

$q=CGI->new;
@a=split(/,/, $q->param('expand'));
if($#a<=0)
{
    %expand=(0 => 1);
}
else
{
    %expand=map { $_, 1 } @a;
}


$c=Collapse->new([@stuff], {%expand});
print <<EOF;
<html>
<head>
<title>Disk Status</title>
</head>

<body bgcolor="fFfFfF">
EOF

$tmp="<img src=\"/images/arrow-collapsed.gif\" border=\"0\" alt=\"Expand\">";
$c->set_html_expand($tmp);
$tmp="<img src=\"/images/arrow-expanded.gif\" border=\"0\" alt=\"Collapse\">";
$c->set_html_collapse($tmp);

$c->print_html;

print "</body></html>\n";
