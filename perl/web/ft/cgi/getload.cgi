#!/usr/bin/perl

sub fx
{
    local($n)=@_;
    $n="0$n" if($n<10);
    return($n);
}

($host, $line, $date)=@ARGV;

# $usage="/home/dustins/bin/usage";
$logs="/home/dustins/logs/bandwidth";

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
<body bgcolor="fFfFff">

<h2>Bandwidth Consumption for $line on $host on $date</h2>
<a href="/cgi-bin/dustin/getload.cgi?$host+$line+$olddate">
Check $hosts's consumption on $line for $olddate</a> (if I have it)

<br>
<pre>

<img src="/cgi-bin/dustin/usage.cgi?$logs/$date/$host+$line">
EOF

# open(IN, "$usage $logs/$date/$host $line|");
# while(<IN>)
# {
#    print;
# }

print <<EOF;
</pre></body></html>
EOF
