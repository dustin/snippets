#!/usr/local/bin/perl
# $Id: append.cgi,v 1.2 1997/12/14 21:32:01 dustin Exp $

push(@INC, "/home/monitor/lib");
require 'cgi-lib.pl';
require 'statlib.pl';

&ReadParse(*input);

sub getthetime
{
    local($hour, $minute, $second)=@_;

    $minute="0$minute" if($minute<10);
    $second="0$second" if($second<10);

    return("$hour:$minute:$second");
}

sub getftoday
{
    local($mon, $day, $year)=@_;

    $mon="0$mon" if($mon<10);
    $day="0$day" if($day<10);
    $year="0$year" if($year<10);

    return("$mon$day$year");
}

sub addtolist
{
    local($fdate, $header, $text)=@_;

    &ensurepath("$appendBase/$fdate");

    open(OUT, ">>$appendBase/$fdate");

print OUT <<EOF;
# $ENV{'REMOTE_HOST'} ($ENV{'REMOTE_ADDR'})
# $ENV{'HTTP_USER_AGENT'}

$header<br>
$text
<hr width="50%">
EOF

    close(OUT);
}

($sec,$min,$hour,$mday,$mon,$year)=localtime(time());

$time=time();
$mon++;
$today="$mon/$mday/$year";
$ftoday=&getftoday($mon, $mday, $year);
$thetime=&getthetime($hour, $min, $sec);
$rightnow="$today $thetime";

print <<EOF;
Content-type: text/html

<html><head><title>Append Results</title></head>
<body bgcolor="fFfFfF">

<h2>Post results</h2>

The following will be added to the display list for $today ($ftoday):

<p>
$rightnow<br>
$input{'text'}
EOF

&addtolist($ftoday, $rightnow, $input{'text'});

print "<br>Carried forward $input{'carryforward'} days\n";

if($input{'carryforward'}>0)
{
    print "<hr width=\"50%\" align=\"left\">\n";
    for($i=1; $i<=$input{'carryforward'}; $i++)
    {
	@a=localtime($time+($i * 86400));
	$fdate=&getftoday($a[4]+1, $a[3], $a[5]);
	$date="$a[4]/$a[3]/$a[5]";
	&addtolist($fdate, "(carried over from $rightnow)", $input{'text'});
	print "<br>Carried over to $date.\n";
    }
}
