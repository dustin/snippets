#!/usr/local/bin/perl

use strict;

# A quick table of notes
my %notes=(
	'A' => 440, 'AS' => 466,
	'B' => 494,
	'C' => 523, 'CS' => 554,
	'D' => 587, 'DS' => 622,
	'E' => 659,
	'F' => 698, 'FS' => 739,
	'G' => 784, 'GS' => 831
);

my $fin=65536;
# my $fout=8000;
# my $fout=44100;
my $fout=8192;
my @basewave=();

# Degrees to radians
sub d2r
{
	my($d)=@_;
	return( ($d/360) * 2 * 3.1415926535897932626);
}

# Get the wave at a given frequency
sub getWave
{
	my($f)=@_;
	my(@r)=();
	my($i);

	for($i=0; $i<=$f; $i++) {
		my $d= 360 * ($i/$f);
		my $r= d2r($d);
		push(@r, sin($r));
	}
	return(@r);
}

# Print out the values of the given wave
sub dumpWave
{
	my(@w)=@_;
	my($i);
	for($i=0; $i<@w; $i++) {
		my $v=128+($w[$i]*128);
		print "$i: $w[$i] $v\n";
	}
}

# Convert the wave to a series of bytes, -1 = 0, 0 = 128, 1 = 256
sub getTone
{
	# Frequency, duration
	my($freq, $t)=@_;
	my(@r, $current, $step);
	# Figure out how many bytes we need to get the given duration
	my $rbytes=$fout*$t;
	# Calculate the step
	$step=($freq/$fout)*$fin;
	# print STDERR "Step is $step for $freq, bytes is $rbytes for $t\n";
	$current=0;
	@r=();
	while(@r < $rbytes) {
		if($current > $fin) {
			# print STDERR "current exceeded $fin:  $current\n";
			# $current = ($current % $fin);
			$current = ($current%$fin)+1;
		}

		my $v=256+($basewave[$current]*256);
		push(@r, $v);

		$current+=$step;
	}
	# Only return the requested amount
	return(@r);
}

#
# Main, play an A minor scale
#

@basewave=getWave($fin);

print "float wave[]={\n";
for(getTone($notes{'F'}, 1)) {
	print "\t$_,\n";
}
print "};\n";
