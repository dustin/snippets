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

my $base=8000;

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
sub wav2bytes
{
	my($t, @w)=@_;
	my($i, $rbytes, $r);
	# Figure out how many bytes we need to get the given duration
	$rbytes=$t*$base;
	$r="";
	while(length($r) < $rbytes) {
		for($i=0; $i<@w; $i++) {
			my $v=128+($w[$i]*128);
			$r.=pack("c", $v);
		}
	}
	# Only return the requested amount
	return(substr($r, 0, $rbytes));
}

# Get a specific frequency at a given duration
sub getTone
{
	my($freq, $dur)=@_;
	my $size=($base * (1/$freq));
	return(wav2bytes($dur, getWave($size)));
}

#
# Main, play an A minor scale
#

my @notes=qw(A B C D E F GS);
for(@notes) {
	print getTone($notes{$_}, 1);
}
# last note in a minor
print getTone($notes{'A'} * 2, 1);
