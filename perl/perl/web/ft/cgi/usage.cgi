#!/usr/local/bin/perl

use GIFgraph::lines;

$oldthrough=0;
$oldtime=0;

format STDOUT=
@<<<<<<<<<<<<<  @###.##      @<<<<<<<<<<<<<<<<<<<<<<<<<
&fixtime($a[1]), $util*100, &printhash($util)
.

format top=
@<<<<<<<<<<<<<  @<<<<<<      @<<<<<<<<<<<<<<<<<<<<<<<<<
"Time",   "Percent", "0%                    100%"
.

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

sub printhash
{
	local($through)=@_;
	local($num, $ans);
	$ans="";
	$num=$util*20;
	for(;$num>0;$num--)
	{
		$ans="$ans#";
	}
	return "$ans";
#	print "\n";
}

open(IN, $ARGV[0]) || die("$ARGV[0]: $!");

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
    $util=(8*(&max( ($a[3]-$old[3]),
		    ($a[4]-$old[4]))/($a[1]-$old[1]))/$a[5]);

    $indiff=$a[3]-$old[3];
    $outdiff=$a[4]-$old[4];
    $Bps=$a[5];
    $tdiff=$a[1]-$old[1];

    $inbps=(( (8*($indiff / $tdiff)) /($Bps))*100);
    $outbps=(( (8*($outdiff / $tdiff)) /($Bps))*100);

    $inbps=0 if($inbps<0);
    $outbps=0 if($outbps<0);

    push(@{$plotdata[0]}, &fixtime($a[1]));
    push(@{$plotdata[1]}, $util*100);
    push(@{$plotdata[2]}, $inbps);
    push(@{$plotdata[3]}, $outbps);

    @old=@a;
}

close(IN);

chdir($libdir);

$my_graph = new GIFgraph::lines(800, 300);

$my_graph->set( 'y_label' => 'Percent Utilization',
                'x_label' => 'Time',
                'title' => "line utilization",
                'y_max_value' => 100,
                'y_min_value' => 0,
		'x_label_skip' => 8,
		'long_ticks' => 1
              );

print "Content-type: img/gif\n\n";

print $my_graph->plot(\@plotdata );
