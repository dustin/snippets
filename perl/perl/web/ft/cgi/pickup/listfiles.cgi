#!/usr/local/bin/perl

push(@INC, "/home/dustins/lib");
require 'filething.pl';

sub showbatches
{
    local($d)=@_;
    print "$d\n<ul>";

    opendir(D, $d);
    while($_=readdir(D))
    {
        next if(/^\./);

        print "<li><a href=\"/cgi-bin/dustin/pickup/listfiles.cgi?$_\">";
        print "$_</a></li>\n";
    }
    closedir(D);

    print "</ul>\n";
}

sub showfiles
{
    local($d, $batch)=@_;

    $d.="/$batch";
    print "$d\n<ul>";

    opendir(D, $d);
    while($_=readdir(D))
    {
        next if(/^\./);

        print "<li><a href=\"/cgi-bin/dustin/pickup/getfile.cgi?$batch/$_\">";
        print "$_</a></li>\n";
    }
    closedir(D);

    print "</ul>\n";
}

$dir="$fileRoot/$ENV{REMOTE_USER}";

print <<EOF;
Content-type: text/html

<html><head><title>File Pickup List</title></head>

<body bgcolor="fFfFfF">

EOF

if($#ARGV>=0)
{
    showfiles($dir, $ARGV[0]);
}
else
{
    showbatches($dir);
}
