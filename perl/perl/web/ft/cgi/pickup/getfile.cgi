#!/usr/local/bin/perl

push(@INC, "/home/dustins/lib");
require 'filething.pl';

$f="$fileRoot/$ENV{'REMOTE_USER'}/$ARGV[0]";

if(-f $f)
{
    print <<EOF;
Content-type: unknown/data

EOF
    open(IN, $f);
    print <IN>;
    close(IN);
}
else
{
    print <<EOF;
Content-type: text/plain

No such file.
EOF
}
