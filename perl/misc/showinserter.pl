#!/usr/local/bin/perl -w

use strict;

my $name=$ARGV[0];
my $user='jennifer';
if(@ARGV>1) {
	$user=$ARGV[1];
}
my $file="/afs/spy.net/home/dustin/public_html/ds/$name";

my @stat=stat($file);
if(!@stat) {
	die("Failed to stat $file: $!\n");
}

my $size=$stat[7];

print "insert into show_distribution(show_id, submitted_to, length)\n"
	. "\tvalues('$name', '$user', $size);\n";
