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
	my($r, $current, $step);
	# Figure out how many bytes we need to get the given duration
	my $rbytes=$fout*$t;
	# Calculate the step
	$step=($freq/$fout)*$fin;
	# print STDERR "Step is $step for $freq, bytes is $rbytes for $t\n";
	$current=0;
	$r="";
	while(length($r) < $rbytes) {
		if($current > $fin) {
			$current = ($current%$fin)+1;
		}

		my $v=128+($basewave[$current]*128);
		$r.=pack("c", $v);
		# my $v=32768+($basewave[$current]*32768);
		# $r.=pack("s", $v);
		# $r.=pack("s", $v);

		$current+=$step;
	}
	# Only return the requested amount
	return($r);
}

#
# Main, play an A minor scale
#

print ".snd";
print pack("N", 56);
print pack("N", 120000);
print pack("N", 2); # format
print pack("N", $fout); # frequency
print pack("N", 1); # channels
print "DLS\0";

@basewave=getWave($fin);

my @notes=qw(A B C D E F GS);
for(@notes) {
	print getTone($notes{$_}, 1);
}
# last note in a minor
print getTone($notes{'A'} * 2, 1);
