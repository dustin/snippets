#!/usr/local/bin/perl

use CGI;

my($q, $module, $dir, $date);

$q=CGI->new;

print $q->header;

# parse PATH_INFO

print $ENV{'PATH_INFO'};
