#!/usr/local/bin/perl -w
#
# $Id: missing.cgi,v 1.1 1998/09/18 08:02:58 dustin Exp $

use CGI;
use strict;

sub readConfig
{
    my(@h, $i, $key, @a, @b);

    $i=-1;
    open(IN, '/usr/people/dustin/prog/perl/web/missing.cf');
    while(<IN>) {
        next if(/^#/);
	next unless(/\w/);
	if(/\[(.*)\]/) {
	    $key=$1;
	    $i++;
	    push(@{$h[$i]}, $key);
	} else {
	    next unless($key=~/\w/);
	    chop;
	    @a=split(/\t/);
	    @b=split('=', $a[1]);

	    push(@{$h[$i]}, [$a[0], @b]);
	}
    }
    close(IN);
    return(@h);
}

sub redirect
{
    my($q, $arg)=@_;

    print $q->redirect($arg);
}

sub doperl
{
    my($q, $arg)=@_;
    my($data);

    open(IN, $arg);
    $data=join('', <IN>);
    close(IN);

    # eval { $data };
    eval $data;
}

sub dofile
{
    my($q, $arg)=@_;
    open(IN, $arg);
    print <IN>;
    close(IN);
}

sub mainloop
{
    my(@cf, $q, $key, $done, @action, %actions);
    $q=CGI->new;
    @cf=readConfig();

    $done=0;
    for $key (0..(@cf-1)){
	if( ($ENV{'SERVER_NAME'}=~/$cf[$key]->[0]/
	     || $cf[$key]->[0] eq '-defaults-')
	     && ($done==0)) {
	    for(1..(@{$cf[$key]}-1)) {
	        if($ENV{'PATH_INFO'}=~/$cf[$key]->[$_]->[0]/) {
		    $done=1;
		    @action=@{$cf[$key]->[$_]};
	            last;
	        } # if
	    } #for
	} #if
    } #for

    # Actions
    %actions=(
        'R' => \&redirect,
	'P' => \&doperl,
	'F' => \&dofile,
    );

    &{$actions{$action[1]}}($q, $action[2]);

} # sub

# Isolation is the key.  :)
mainloop();
