#!/usr/local/bin/perl

push(@INC, "/home/dustins/lib");
require 'cgi-lib.pl';
require 'filething.pl';

$batchId;

sub notify
{
    local($to)=@_;
    print "Sending notify to $userMap{$to}<br>\n";
    open(MAIL, "|/usr/lib/sendmail -oi -t");
    print MAIL<<EOF;
From: Dustin's File Upload Utility <dustin\@cybersource.com>
To: $userMap{$to}
Subject: New file.

The batch $batchId has been uploaded for you.
EOF
    close(MAIL);
}

sub storefile
{
    local($to, $sname, $name)=@_;
    local($fn);

    $fn="$fileRoot/$to/$batchId/$name";
    &ensurePath($fn);

    print "<br>Storing file into $fn<br>\n";
    system("mv $sname $fn");
}

sub getrfn
{
    local($cfn)=@_;
    local($last);

    $last=rindex($cfn, "/");
    $rfn=substr($cfn,$last+1);

    $cfn=$rfn;
    $last=rindex($cfn, "\\");
    $rfn=substr($cfn,$last+1);

    return($rfn);
}

sub doheader
{
print <<EOF;
Content-type: text/html

<html><head><title>File upload</title></head>

<body bgcolor="fFfFfF">

EOF
}

$cgi_lib::writefiles="/tmp";
$cgi_lib::maxdata=50000000;

&ReadParse(\%in, \%cgi_cfn, \%cgi_ct, \%cgi_sfn);

$sfn=$cgi_sfn{'thisfile'};
$rfn=&getrfn($cgi_cfn{'thisfile'});

&doheader;
$batchId=&newId($in{'to'});

if(defined($userMap{$in{'to'}}))
{
    storefile($in{'to'}, $cgi_sfn{'thisfile'}, $rfn);
    notify($in{'to'});
}
else
{
    print "Unknown user:  $in{'to'}\n";
    unlink($cgi_sfn{'thisfile'});
}
