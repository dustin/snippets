#!/usr/local/bin/perl -- -*-perl-*-
#
# $Id: ldform.cgi,v 1.1 1997/12/12 21:36:01 dustin Exp $
#
$SIG{'ALRM'} = 'handler';
$deb = 1;

$site="hitchcock.cybersource.com";
  local($sockaddr,$there,$response,$tries) = ("Snc4x8");
  alarm(15);
  $port=23;
  $there = pack($sockaddr,2,$port,&getaddress($site));
  if (!socket(S,2,2,6)) { &debug("socket open fail $!"); return(0); }
  if (!connect(S,$there)) { &debug("No connect $site"); return(0); }
  select(S);$|=1;$/=" ";
  select(STDOUT);
  while(<S>)
  {
	$temp .= $_;
	last if $temp =~ /word:/i;
  }
  print S "a!ta\n";
  &getprompt;
  print S "pager lines 0\n";
  &getprompt;
  print S "show real\n";
  $real = &getprompt;
  print S "show virtual\n";
  $virtual = &getprompt;
  print S "show conn\n";
  $constat = &getprompt;
  close(S);
  alarm(0);

$out="Content-type: text/html\n\n";

$out.="<html><head><title>Load Director Stats</title></head><body>\n";

$out.="<FORM method=post action=/cgi-bin/stats/ldcontrol.cgi>";
$out.="<table border=\"2\"><tr><td valign=\"top\">\n";
$out.="<b>Server State - virtual servers</b><br>\n";
$out.="<b><font color=red>Warning:</font></b><br>Taking virtual servers out of service will full disable the web site in question";
$out.="<table border=\"0\">\n";
$out.="<tr><th>Server</th><th colspan=2>State</th><th>\n";
$out.="Load</th></tr>\n";


@virtual = sort vbyload split(/\n/,$virtual);
$vscale = 2;
if ($vmax > 100)
{
	$vscale = 200 / $vmax;
}
foreach (@virtual)
{
        local($server,$port,$conna,$state,$stick,$pred,$xa,$isc,$osc);
        s/^\s+//;
        ($server,$port,$state,$conn,$stick,$pred) = split(/\s+/,$_);

        next unless $stick =~ /\d/;
	$isc = "checked" if ($state =~ /IS/);
	$osc = "checked" if ($state =~ /OOS/);
        $out .= "<tr><th align=left>$server:$port</th>";
	$out .= "<th>IS<input type=radio name=\"virtual-$server-$port\" value=IS $isc></th><th>OOS<input type=radio name=\"virtual-$server-$port\" value=OOS $osc></th><input type=hidden name=\"old-$server-$port\" value=$state>";
	$out .= "<td>$conn</td></tr>\n";

}
$out .= "</table>";
$out .= "<hr>";
$out .= "<b>Enable Password</b> <Input length=12 type=password name=enable>\n";
$out .= "<input type=submit value=Apply><br><hr>";
$constat =~ s/show conn/<b>Connection Stats<\/b>/;
$constat =~ s/\n/<br>/g;
$out .= "$constat";
$out .= "<table><th colspan=2>Key</th></tr>";
$out .= "<tr><th bgcolor=C0FFC0><font color=green>IS</font></th><td>In Service - server is available for and accepting requests</td></tr>\n";
$out .= "<tr><th>FAILED</th><td>In Service but not accepting requests, removed from rotation</td></tr>\n";
$out .= "<tr><th>TESTING</th><td>In Service testing for recovery from FAIL condition</td></tr>\n";
$out .= "<tr><th bgcolor=FFC0C0><font color=red>OOS</font></th><td>Administrativly Out Of Service</td></tr>\n";
$out .= "</table>";
$out .= "<b><a href=/stats/load.html>Back to load monitor</a>";

$out .= "</td><td valign=top>";
$out .= "<b>Server State - real servers</b>";
$out .= "<table border=0>\n<tr><th>Server</th><th colspan=2>State</th><th colspan =1>Load</th></tr>";

@real = sort byload split(/\n/,$real);
$rscale = 6;
if ($rmax > 20)
{
        $rscale = 120.0 / $rmax ;
}
foreach (@real)
{
        local($server,$port,$conn,$state,$isc,$osc,$thresh,$reas,$reset);
	s/^\s+//;
	($server,$port,$conn,$state,$thresh,$reas,$reset) = split(/\s+/,$_);

	next unless $reset =~ /\d/;
	if ($last =~ /./ && $server !~ /^$last/)
	{
		$out .= "<tr><td colspan=4><hr></td></tr>\n";
	}
	$server =~ /([^-]*).*/;  $last = $1;
	$isc = "checked" if ($state =~ /IS/);
	$osc = "checked" if ($state =~ /OOS/);
	$out .= "<tr><th align=left>$server:$port</th>";
	$out .= "<th>IS<input type=radio name=\"real-$server-$port\" value=IS $isc></th><th>OOS<input type=radio name=\"real-$server-$port\" value=OOS $osc></th><input type=hidden name=\"old-$server-$port\" value=$state>";
	$out .= "<td>$conn</td></tr>\n";

}
$out .= "<!-- rmax = $rmax rscale = $rscale-->\n";
$out .= "</table>";
$out .= "</td></tr></table>";
$out .= "</form>";
$out =~ s/>IS(.*=IS checked><)/ bgcolor=C0FFC0><font color=green>IS<\/font>$1/g;
$out =~ s/>OOS(.*=OOS checked><)/ bgcolor=FFC0C0><font color=red>OOS<\/font>$1/g;
$out =~ s/>FAILED</ bgcolor=FF0000><font color=white>FAIL<\/font></g;
$out =~ s/>TESTING</ bgcolor=FF8080><font color=white>TEST<\/font></g;
$out =~ s/width=0/width=1/g;

# evil hackery
$out =~ s/:80</</g;
$out =~ s/:443/(SSL)/g;
print $out;

sub getaddress {
  local($host) = @_;
  $host =~ s#techweb.cmp.com#192.216.46.14#;
  local(@ary);
  @ary = gethostbyname($host);
  return(unpack("C4",$ary[4]));
}
sub handler {
        close(S);
	print "Timeout";
        $timeout = "yes";
}
sub debug {
        local($msg) = @_;
        $ref =~ s/^O//;
        $deb == 1 &&    print STDERR "$msg\n";
        return 1;
}

sub getprompt {
  local $temp;
  while(<S>)
  {
        $temp .= $_;
        last if $temp =~ /hitchcock> /i;
  }
  $temp =~ s/hitchcock> //i;
  return $temp;
}

sub byload {
	local($servera,$porta,$conna,$state,$thresh,$reas,$reset,$xa);
	local($serverb,$portb,$connb,$state,$thresh,$reas,$reset,$xb);
	$xa = $a;
	$xb = $b;
	$xa =~ s/^\s+//;
	$xb =~ s/^\s+//;
	  ($servera,$porta,$conna,$state,$thresh,$reas,$reset) = split(/\s+/,$xa);
	  ($serverb,$portb,$connb,$state,$thresh,$reas,$reset) = split(/\s+/,$xb);
	$rmax = $connb if ($connb > $rmax);
	$rmax = $conna if ($conna > $rmax);
	if ($servera eq $serverb)
	{
		return 	($porta <=> $portb);
	} else {
		return 	($servera cmp $serverb);
	}
}

sub vbyload {
        local($server,$port,$conna,$state,$stick,$pred,$xa);
        local($server,$port,$connb,$state,$stick,$pred,$xb);
        $xa = $a;
        $xb = $b;
        $xa =~ s/^\s+//;
        $xb =~ s/^\s+//;

          ($server,$port,$state,$conna,$stick,$pred) = split(/\s+/,$xa);
          ($server,$port,$state,$connb,$stick,$pred) = split(/\s+/,$xb);
	$vmax = $connb if ($connb > $vmax);
	$vmax = $conna if ($conna > $vmax);
        $connb <=> $conna;
}

