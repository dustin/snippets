#!/usr/local/bin/perl
# $Id: makeapie.cgi,v 1.1 1997/12/12 21:36:01 dustin Exp $

use GIFgraph::pie;

$im=new GIFgraph::pie(300, 300);

@args=split(/\+/, $ENV{QUERY_STRING});

$args[0]=~s/%([A-Fa-f0-9]{2})/pack("c",hex($1))/ge;

for(1..$#args)
{
    @a=split(/=/, $args[$_]);
    $a[0]=~s/%([A-Fa-f0-9]{2})/pack("c",hex($1))/ge;
    $a[1]=~s/%([A-Fa-f0-9]{2})/pack("c",hex($1))/ge;

    push(@{$plotdata[0]}, $a[0]);
    push(@{$plotdata[1]}, $a[1]);
}

$im->set(
        'title' => "$args[0]",
        'dclrs' => [ 'lblue', 'green', 'red', 'yellow' ],
	'3d' => 1,
);

print "Content-type: image/gif\nPragma: no-cache\n\n";

print $im->plot(\@plotdata);

exit 0;
