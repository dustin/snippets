#!/usr/local/bin/perl
# $Id: state.cgi,v 1.2 1997/12/14 21:31:58 dustin Exp $

push(@INC, "/home/monitor/lib");
require 'statlib.pl';

sub fx
{
    local($n)=@_;
    $n="0$n" if($n<10);
    return($n);
}

($host, $obj, $date)=@ARGV;

if($date eq "")
{
    @a=localtime();
    $a[4]++;
    $a[4]="0$a[4]" if($a[4]<10);
    $a[3]="0$a[3]" if($a[3]<10);
    $date="$a[4]$a[3]$a[5]";
}

# $usage="/home/dustins/bin/usage";

@days=(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

$date=~/(..)(..)(..)/;

$m=$1;
$d=$2-1;
$y=$3;

if($d==0)
{
    $m--;
    if($m==0)
    {
       $y--;
       $m=12;
    }
    $d=$days[$m];
}

$olddate=$m.&fx($d).&fx($y);

print <<EOF;
Content-type: text/html

<html><head><title>$obj for $host on $date</title>
<META HTTP-EQUIV="Refresh" CONTENT="300">
<body bgcolor="fFfFff">

<h2$obj for $host on $date</h2>
<a href="/cgi-bin/stats/dustin/state.cgi?$host+$obj+$olddate">
Check $hosts's $obj for $olddate</a> (if I have it)
<p>

<center>
<img src="$grState?$date/$host+$obj">
</center>
<p>
</body></html>
EOF
