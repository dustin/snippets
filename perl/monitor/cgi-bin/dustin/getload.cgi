#!/usr/local/bin/perl
# $Id: getload.cgi,v 1.2 1997/12/14 21:31:57 dustin Exp $

push(@INC, "/home/monitor/lib");
require 'statlib.pl';

sub fx
{
    local($n)=@_;
    $n="0$n" if($n<10);
    return($n);
}

($host, $line, $date)=@ARGV;

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

<html><head><title>Bandwidth Consumption for $line on $host on $date</title>
<META HTTP-EQUIV="Refresh" CONTENT="300">
<body bgcolor="fFfFff">

<h2>Bandwidth Consumption for $line on $host on $date</h2>
<a href="/cgi-bin/stats/dustin/getload.cgi?$host+$line+$olddate">
Check $hosts's consumption on $line for $olddate</a> (if I have it)
<p>

<center>
<img src="$grUsage?$date/$host+$line">
</center>
<p>
Colors:<br>
<blockquote>
<font color="#1111ff">
Blue:    Output<br>
</font>
<font color="#ff1111">
Red:     Input<br>
</font>
<font color="#7f117f">
Purple:  Total
</font>
</blockquote>
</body></html>
EOF
