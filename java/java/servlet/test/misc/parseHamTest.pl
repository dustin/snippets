#!/usr/local/bin/perl

use strict;

my %answer_map=('A' => 0, 'B' => '1', 'C' => 2, 'D' => 3);

sub findQuestion {
	my(@rv);
	while($_=<>) {
		if(/T(\w+)\s+@(\w+)\s+\(([ABCD])\)/) {
			@rv=("T" . $1, $answer_map{$3});
			last;
		}
	}
	return(@rv);
}

my($question_no, $answer);

while(($question_no, $answer)=findQuestion()) {
	# print "Question $question_no answer $answer\n";
	my $question="";
	my(@answers);
	my($answer_id);
	$answer_id=-1;

	while($_=<>) {
		chomp; chomp;
		# Trim the ends
		s/\s+$//g;

		# Commenty things.
		next if(/^\*/);

		if($_ eq "") {
			# End of answers
			last;
		}

		if($_=~/^[ABCD]\./) {
			$answer_id++;
			$answers[$answer_id]=substr($_, 4);
		} else {
			if($answer_id==-1) {
				$question.=$_ . " ";
			} else {
				$answers[$answer_id].=$_;
			}
		}
	}

	print "$question\n";
	print "--\n";
	print join("\n", @answers);
	print "\n--\n";
	print ($answer+1);
	print "\n-----------\n";

# 	print "$question\n\n";
	# for(0..3) {
		# if($answer == $_) {
			# print "$_ --> ";
		# } else {
			# print "$_     ";
		# }
		# print $answers[$_] . "\n";
	# }
	# print "-------------------------------------------\n";
}
