#!/usr/local/bin/perl -w
#
# $Id: missing.cgi,v 1.4 1998/09/19 03:42:19 dustin Exp $

use CGI;
use LWP::UserAgent;
use strict;

sub readConfig
{
    my(@h, $i, $key, @a);

    $i=-1;
    open(IN, '/home/www/data/conf/missing.cf');
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
	    push(@{$h[$i]}, [$a[0], split('=', $a[1])]);
	}
    }
    close(IN);
    return(@h);
}

sub doredirect
{
    my($q, $arg)=@_;
    print $q->redirect($arg);
}

sub doperl
{
    my($q, $arg)=@_;
    do($arg);
}

sub dofile
{
    my($q, $arg)=@_;
    print $q->header;
    open(IN, $arg);
    print <IN>;
    close(IN);
}

sub dofetch
{
    my($q, $arg)=@_;
    my($ua, $req, $res);
    $ua=LWP::UserAgent->new;
    $ua->agent('DustInvProxy/2.0', $ua->agent);
    $req=HTTP::Request->new('GET', $arg);
    $res=$ua->request($req);
    print $q->header($res->header('Content-Type'));
    print $res->content;

}

sub mainloop
{
    my(@cf, $q, $key, $done, @action, %actions, @stuff, $path);
    $q=CGI->new;
    @cf=readConfig();

    # This works a little differently for Apache and Netscape
    if(defined($ENV{'PATH_INFO'})) {
        $path=$ENV{'PATH_INFO'};
    } else {
        $path=$ENV{'REQUEST_URI'};
    }

    $done=0;
    for $key (0..(@cf-1)){
	if( ($ENV{'SERVER_NAME'}=~/$cf[$key]->[0]/
	     || $cf[$key]->[0] eq '-defaults-') 
	     && ($done==0)) {
	    for(1..(@{$cf[$key]}-1)) {
	        if($path=~/$cf[$key]->[$_]->[0]/) {
		    # Check this out, simulate perl with perl.  :)
		    @stuff=($1, $2, $3, $4, $5, $6, $7, $8, $9);
		    @action=@{$cf[$key]->[$_]};
		    $action[2]=~s/\$(\d+)/$stuff[$1-1]/g;
		    $done=1;
	            last;
	        } # if
	    } #for
	} #if
    } #for

    # Actions
    %actions=(
        'redirect' => \&doredirect,
	'perl' =>     \&doperl,
	'file' =>     \&dofile,
	'fetch' =>    \&dofetch,
    );

    if(defined($actions{$action[1]}) ) {
        &{$actions{$action[1]}}($q, $action[2]);
    } else {
        die("Can't do ``$action[1]''\n");
    }

} # sub

# Isolation is the key.  :)
mainloop();
