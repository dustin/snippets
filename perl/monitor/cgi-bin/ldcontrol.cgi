#!/usr/local/bin/perl -- -*-perl-*-
#
# $Id: ldcontrol.cgi,v 1.1 1997/12/12 21:36:01 dustin Exp $
#
$SIG{'ALRM'} = 'handler';
$deb = 1;

require "cgi-lib.pl";
ReadParse(*DATA);

$out="Content-type: text/plain\n\n";
foreach $k (keys %DATA)
{
	my($t,$n,$s);
	($t,$n) = split(/-/,$k,2);
	if ($t =~ /old/) {
		$old{"$n"} = $DATA{$k};
	} 
		
}
foreach $k (keys %DATA)
{
	my($t,$n,$p,$command);
	($t,$n) = split(/-/,$k,2);
	next if $t =~ /old/;
	next if $k =~/enable/;
	next if $old{"$n"} eq $DATA{$k};
	$n =~ s/(.*)-(\d*)/$1/;
	$p = $2;
	$command = "$DATA{$k} $t $n $p";
	$command =~ s/OOS/oos/;
	$command =~ s/IS/is/;
	push(@cmds,$command);
}
$DATA{enable} = "invalid" unless $DATA{enable} =~ /./;
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
$out .=  &docmd(S,"","hitchcock. ");
$out .=  &docmd(S,"pager lines 0","hitchcock. ");
$out .=  &docmd(S,"en $DATA{enable}","hitchcock. ");
foreach $command (@cmds)
{
	$out .=  &docmd(S,"$command","hitchcock. ");
}
$out .=  &docmd(S,"wr m","hitchcock. ");
$out .=  &docmd(S,"dis","hitchcock. ");
$out .=  &docmd(S,"quit","hitchcock. ");

#print $out;
if ($out =~ /invalid/)
{
	print $out;
} else {
	print "Location: $ENV{SERVER_URL}/cgi-bin/stats/ldform.cgi\n\n"
}

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

sub docmd {
  local($FILE,$cmd,$prompt) = @_;
  local $temp;
  print $FILE "$cmd\n" if $cmd =~ /./;
  while(<$FILE>)
  {
        $temp .= $_;
        last if $temp =~ /$prompt/i;
  }
#  $temp =~ s/$prompt//i;
 # $temp =~ s/$cmd//i;
  return $temp;
}

