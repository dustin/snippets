#!/usr/local/bin/perl
# $Id: grState.cgi,v 1.1 1997/12/12 21:36:01 dustin Exp $

use GIFgraph::lines;

push(@INC, "/home/monitor/lib");
require 'statlib.pl';

@fields=qw(
        hostname time
	load1 load5 load15
        cp_user cp_nice cp_system cp_idle
        gcp_user gcp_nice gcp_system gcp_idle
        pack_in pack_out gpack_in gpack_out
	swap_in swap_out gswap_in gswap_out
	page_in page_out gpage_in gpage_out
	ping total
);

for(0..$#fields)
{
    $fields{$fields[$_]}=$_;
}

$oldthrough=0;
$oldtime=0;

@d=localtime();
$d[4]++;
$d[3]="0$d[3]" if($d[3]<10);
$today="$d[4].$d[3].". 1900+$d[5];

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

if($date=~/([0-9]{2})([0-9]{2})([0-9]{2})/)
{
    $date=join('.', $1, $2, 1900+$3);
}

# only take the last thing after slashes...
$name=pop(@a);
$f="$date/$name";

open(IN, "$statelogs/$f") || die("$statelogs/$f: $!");

if($#ARGV>0)
{
	$field=$ARGV[1];
}

@old=split(';');

while(<IN>)
{
    @a=split(';');
    $host=$a[0];

    push(@{$plotdata[0]}, &fixtime($a[1]));
    push(@{$plotdata[1]}, $a[$fields{$field}]);

    $ymax = $a[$fields{$field}] if ($a[$fields{$field}] > $ymax);
}

close(IN);

$ymax = int($ymax)+1;
# $ymax = 100 if ($ymax > 100);

chdir($libdir);

$my_graph = new GIFgraph::lines(800, 300);

$my_graph->set( 'y_label' => "$field for $host",
                'x_label' => 'Time',
                'title' => "$field for $host",
                'y_max_value' => $ymax,
                'y_min_value' => 0,
		'x_label_skip' => 8,
		'long_ticks' => 1,
                'dclrs' => ['purple', 'lred', 'lblue']
              );

print "Content-type: image/gif\n\n";

print $my_graph->plot(\@plotdata );
