#!/usr/local/bin/perl
# $Id: showappendlist.cgi,v 1.2 1997/12/14 21:31:52 dustin Exp $

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

<ul>
EOF

%a=&getlist;

foreach $key (sort(keys(%a)))
{
    print " <li><a href=\"/cgi-bin/stats/showxappend.cgi?$key\">$a{$key}</a></li>\n";
}

print <<EOF;
</ul>

<hr width="50%" align="left">

</body></html>
EOF
