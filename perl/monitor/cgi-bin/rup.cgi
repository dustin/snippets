#!/usr/local/bin/perl
# $Id: rup.cgi,v 1.2 1997/12/14 21:31:48 dustin Exp $

push(@INC, "/home/monitor/lib");
require 'statlib.pl';

%list=();

# should get days, misc

sub processtime
{
    local(@stuff)=@_;
    local(@ret, $flag, $days, $hours, $minutes);

    ($flag, $days, $hours, $minutes)=(0,0,0,0);

    $_=$stuff[0];

    if(/([0-9]+) day/)
    {
	$days=$1;
	$flag=1;
    }
    elsif(/([0-9]+):([0-9]+)/)
    {
	$hours=$1;
	$minutes=$2;
    }

    if($flag == 1)
    {
	$_=$stuff[1];

	if(/([0-9]+):([0-9]+)/)
	{
	    $hours=$1;
	    $minutes=$2;
	}
	elsif(/([0-9]+) min/)
	{
	    $minutes=$1;
	}
    }

    $days+=0;
    $hours+=0;
    $minutes+=0;

    @ret=($days, $hours, $minutes);

    return($flag, @ret);
}

# takes hostname, days, misc, load5, load10, load15

sub getinfo
{
    local(@stuff)=@_;
    local($n, $hostname, @uptime, @tmp, $flag);

    $hostname=shift(@stuff);
    $hostname=~s/([^\.]+).+/$1/;

    ($flag, @uptime)=&processtime(@stuff[0], $stuff[1]);

    shift(@stuff);
    if($flag==1)
    {
	shift(@stuff);
    }

    $list{$hostname}=join("\0", @uptime, @stuff);
}

# sort compare

sub funky
{
    @a=split("\0", $list{$a});
    @b=split("\0", $list{$b});

    if($a[0] != $b[0])
    {
	return($b[0] <=> $a[0]);
    }
    elsif($a[1] != $b[1])
    {
	return($b[1] <=> $a[1]);
    }
    elsif($a[2] != $b[2])
    {
	return($b[2] <=> $a[2]);
    }
    else
    {
        return($a cmp $b);
    }
}

sub displaythem
{
    local(@tmp, $uptime, $tmp, $key);
    local(@list);

    @list=sort funky keys(%list);

    print "<table border=\"3\">\n";
    print "<tr><th>machine</th><th>uptime</th><th>load 1m</th><th>load 5m</th><th>load 15m</th>\n";

    foreach $key (@list)
    {
        $tmp=$key;
        $tmp=~s/\s*//g;
	print "<tr><td><a href=#$tmp>$tmp</a></td>\n";
	@tmp=split("\0", $list{$key});
	$uptime="$tmp[0] days, $tmp[1] hours, $tmp[2] minutes";
	shift(@tmp); shift(@tmp); shift(@tmp);
	unshift(@tmp, $uptime);

	for $tmp (0..$#tmp)
	{
	    print "\t<td>$tmp[$tmp]</td>\n";
	}
	print "</tr>\n";
    }

    print "</table>";
}

print <<EOF;
Content-type: text/html

EOF

open(IN, $rmonList);

while(<IN>)
{
    chop;
    $args .= " $_";
}

close(IN);

open(IN, "rup $args |");

while(<IN>)
{
    chop;

    s/up/,/;
    s/load average://;
    @a=split(/,/);

    for $n (0..$#a)
    {
	$a[$n]=~s/\s+(.+\w)/$1/g;
    }
    getinfo(@a);
}

close(IN);

&displaythem;
