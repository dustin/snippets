#!/usr/bin/perl
# $Id: showappendlist.cgi,v 1.3 1997/12/31 19:37:50 dustin Exp $

use CGI;
use Collapse;

push(@INC, "/home/monitor/lib");
require 'statlib.pl';

sub getrealdate
{
    local($fdate)=@_;

    $fdate=/(..)(..)(..)/;

    return("$1/$2/$3");
}

sub getlist
{
    local(@a, %h, $f);

    opendir(D, $appendBase);
    while($f=readdir(D))
    {
        next if($f=~/^\./);
        push(@a, $f);
    }
    closedir(D);

    foreach(@a)
    {
        $h{$_}=&getrealdate($_);
    }

    return(%h);
}

print <<EOF;
Content-type: text/html

<html><head><title>Append List</title></head>
<body bgcolor="fFfFfF">

<h2>List of Append Files</h2>

EOF

$i=1;
$j=0;

$oldyear="";
%a=&getlist;

push(@stuff, "Dates");

foreach $key (sort(keys(%a)))
{
    @a=split(/\//, $a{$key});
    if($a[2] ne $oldyear)
    {
        $oldmonth="";
        $oldyear=$a[2];
        $i++ unless($oldyear eq "");
        push(@{$stuff[$i]}, "$a[2]");

    }

    if($a[0] ne $oldmonth)
    {
        $oldmonth=$a[0];
        if($oldclass ne "")
	{
	    $j=0;
	}
	else
	{
	    $j++;
	}
        push(@{$stuff[$i][$j]}, $a[0]);
    }

    $tmp ="<a href=\"/cgi-bin/stats/showxappend.cgi?$key\">$a{$key}</a>\n";
    push(@{$stuff[$i][$j]}, $tmp);
}

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
$tmp="<img src=\"/images/arrow-collapsed.gif\" border=\"0\" alt=\"Expand\">";
$c->set_html_expand($tmp);
$tmp="<img src=\"/images/arrow-expanded.gif\" border=\"0\" alt=\"Collapse\">";
$c->set_html_collapse($tmp);
# $c->expandall();
$c->print_html;

print <<EOF;

<hr width="50%" align="left">

</body></html>
EOF
