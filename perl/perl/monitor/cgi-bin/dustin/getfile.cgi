#!/usr/local/bin/perl
# $Id: getfile.cgi,v 1.1 1997/12/12 21:36:01 dustin Exp $

&ReadParse(\%in, \%cgi_cfn, \%cgi_ct, \%cti_sfn);

$cgi_lib::writefiles="/tmp";

require 'cgi-lib.pl';

$sfn=$cgi_sfn{'thisfile'};
$cfn=$cgi_cfn{'thisfile'};

$last=rindex($cfn, "/");
$rfn=substr($cfn,$last+1);

print <<EOF;
Content-type: text/html

<html><head><title>Form thingy</title></head>

<body bgcolor="fFfFfF">

I wonder if that worked...

EOF

print &PrintVariables(*in);
