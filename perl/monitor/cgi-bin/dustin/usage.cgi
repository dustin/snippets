#!/usr/local/bin/perl
# $Id: usage.cgi,v 1.2 1997/12/14 21:31:59 dustin Exp $

use GIFgraph::lines;

push(@INC, "/home/monitor/lib");
require 'statlib.pl';

$oldthrough=0;
$oldtime=0;

@d=localtime();
$d[4]++;
$d[3]="0$d[3]" if($d[3]<10);
$today="$d[4]$d[3]$d[5]";

sub max
{
    local($a, $b)=@_;

    return($a) if($a>$b);
    return($b);
}

sub fixtime
{
	local($seconds)=@_;

	($sec, $min, $hour, $mday, $mon)=localtime($seconds);
	$min="0$min" if($min < 10);
	$hour="0$hour" if($hour < 10);
	$mday="0$mday" if($mday < 10);
	$mon++;
	# return "$mon/$mday $hour:$min";
	return "$hour:$min";
}

@a=split(/\//, $ARGV[0]);

if($a[0] eq "today")
{
    $date=$today;
}
else
{
    $date=$a[0];
}

# only take the last thing after slashes...
$name=pop(@a);
$f="$date/$name";

open(IN, "$BWLOGS/$f") || die("$BWLOGS/$f: $!");

if($#ARGV>0)
{
	$tty=$ARGV[1];
}

while(<IN>)
{
	next unless(/$tty/);
	last;
}

@old=split(';');

while(<IN>)
{
    next unless(/$tty/);

    @a=split(';');

    if($a[8]=~/[A-z0-9]/)
    {
        $linename=@a[8];
    }
    else
    {
        $linename=@a[2];
    }

    $indiff=$a[3]-$old[3];
    $outdiff=$a[4]-$old[4];
    $Bps=$a[5];
    $tdiff=$a[1]-$old[1];

    if($#a>=7)
    {
	$inbps=100*(($a[6])/$Bps);
	$outbps=100*(($a[7])/$Bps);
    }
    else
    {
        $inbps=(( (8*($indiff / $tdiff)) /($Bps))*100);
        $outbps=(( (8*($outdiff / $tdiff)) /($Bps))*100);
    }

    $inbps=0 if($inbps<0);
    $outbps=0 if($outbps<0);
    $util=$inbps+$outbps;
    $util=0 if($util<0);
    $util=100 if($util>100);

    push(@{$plotdata[0]}, &fixtime($a[1]));
    push(@{$plotdata[1]}, $util);
    push(@{$plotdata[2]}, $inbps);
    push(@{$plotdata[3]}, $outbps);

    $ymax = $util if ($util > $ymax);

    @old=@a;
}

close(IN);

$ymax = int($ymax)+1;
$ymax = 100 if ($ymax > 100);

chdir($libdir);

$my_graph = new GIFgraph::lines(800, 300);

$my_graph->set( 'y_label' => 'Percentage Utilization',
                'x_label' => 'Time',
                'title' => "line utilization percentage by".
                           "time for $name $linename",
                'y_max_value' => $ymax,
                'y_min_value' => 0,
		'x_label_skip' => 16,
		'long_ticks' => 1,
                'dclrs' => ['purple', 'lred', 'lblue']
              );

print "Content-type: image/gif\nPragma: no-cache\n\n";

print $my_graph->plot(\@plotdata );
