#!/usr/local/bin/perl
# $Id: pixanalyze.cgi,v 1.1 1997/12/12 21:36:01 dustin Exp $

@Okports=(
    "123/udp",
    "137/udp",
    "113/tcp",
    "37/udp",
    "25/tcp",
);
@RedFlag=(
	110,
	109,
	23,
	22,
	69,
	512,513,514,
);

%Fromstat=();
%Portstat=();
%Services=();
%Tomap=();

sub isin
{
    local($what, @a)=@_;
    local($ret);

    $ret=0;

    foreach(@a)
    {
	if($_ == $what)
	{
	    $ret=1;
	    last;
	}
    }

    return($ret);
}

sub lognet
{
    local($src, $port, $prot)=@_;
    local(@a, $srcnet, $svc);

    return if($port>32768 && $prot eq "udp");

    if(isin("$port/$prot", @Okports))
    {
	$Okcount{"$port/$prot"} += 1;
	return;
    }

    $svc="$port/$prot";
    $svc="$Services{$svc}/$prot" if(defined($Services{$svc}));

    # Fromstats

    if(defined($Fromstat{$src})) { $Fromstat{$src}[0]++; }
        else { $Fromstat{$src}[0]=1; }

    if(defined($Fromstat{$src}[1]{$svc})) { $Fromstat{$src}[1]{$svc}++; }
        else { $Fromstat{$src}[1]{$svc}=1; }

    # Portstats

    if(defined($Portstat{$svc})) { $Portstat{$svc}[0]++; }
        else { $Portstat{$svc}[0]=1; }

    if(defined($Portstat{$svc}[1]{$src})) { $Portstat{$svc}[1]{$src}++; }
        else { $Portstat{$svc}[1]{$src}=1; }

    # Tomap

    if(defined($Tomap{"$src:$svc"}{$dst})) { $Tomap{"$src:$svc"}{$dst}++; }
	else { $Tomap{"$src:$svc"}{$dst}=1; }
}

sub ReadServices
{
    local(@a);

    open(SRV, "/etc/services");
    while(<SRV>)
    {
	chop;

	next if(/^#/);
	next if(!/[A-z0-9]/);

	@a=split(/\s+/);

	$Services{$a[1]}=$a[0];
    }
    close(SRV);
}

sub showentry
{
    local($date, $pix, $action, $proto, $src, $srcport, $dst, $dstport)=@_;
    local($f);
    if (isin($dstport,@RedFlag))
    {
	my($svc,$host);
	$svc="$dstport/$proto";
	$svc="$Services{$svc}/$proto" if(defined($Services{$svc}));
	$alert .= "$date $src attempted $svc to $dst\n";
    }
    lognet($src, $dstport, $proto);
}

sub byval_from
{
    return($Fromstat{$b}[0] <=> $Fromstat{$a}[0]);
}

sub byval_port
{
    return($Portstat{$b}[0] <=> $Portstat{$a}[0]);
}

sub byval_port2
{
    return($Portstat{$sortval}[1]{$b} <=> $Portstat{$sortval}[1]{$a});
}

sub byval_to
{
    return($Tomap{"$list[$_]:$port"}{$b} <=> $Tomap{"$list[$_]:$port"}{$a});
}

# Keen perl markup stuff

sub colored
{
    my($color, $text)=@_;
    my($ret);

    if($htmlOutput)
    {
	$ret="<font color=\"$color\">$text</font>";
    }
    else
    {
	$ret=$text;
    }

    return($ret);
}

sub bold
{
    my($text)=@_;
    my($ret);

    if($htmlOutput)
    {
	$ret="<b>$text</b>";
    }
    else
    {
	$ret=uc("*** $text ***");
    }
    return($ret);
}

$sortval=0;

if(@ARGV[0] eq "")
{
    $infile="/var/log/pixlog";
    $htmlOutput=1;
}
else
{
    $infile=$ARGV[0];
    $htmlOutput=0;
}

if($htmlOutput)
{
print <<EOF;
Content-type: text/html

<pre>
EOF
}

&ReadServices;

open(IN, $infile);

while(<IN>)
{
	# stuff to ignore as not important to us
    next if /302001/;  #Build Connection
    next if /302002/;  #Tear down
    next if /304001/;  #http fetch
    next if /305002/;
    #next if /202002/;
    next if /303002/;	#URL fretch
    next if /106011/;   #Same interface transit
    @a=split(/[\s\/]+/);

    if($#a>=11)
    {
        $date="$a[0] $a[1] $a[2]";
        if(/TCP/)
	{
            showentry($date, $a[3], $a[8], "tcp", $a[10], $a[11],
		      $a[13], $a[14]);
	}
	elsif (/UDP/)
	{
            showentry($date, $a[3], $a[5], "udp", $a[9], $a[10],
		      $a[12], $a[13]);
        }
	else
	{
	    $huh .= $_;
	}
    }
}

close(IN);

if ($alert) {
	print(colored(red, bold("RED FLAG ITEMS")) . "\n");
	print $alert;
	print "\n";
}
@list=sort byval_from (keys(%Fromstat));

print(bold("The following ports are being ignored") . "\n");
foreach $svc (@Okports)
{
    my ($port,$prot) = split(/\//,$svc,2);
    $svc="$Services{$svc}/$prot" if(defined($Services{$svc}));
    print "$svc (" . $Okcount{"$port/$prot"} , " attempts)\n";
}
print "\n";
print(bold("Top 20 troublemakers:") . "\n");

for(0..19)
{
    last if($_ > $#list);
    # print "$list[$_] ...\t$Fromstat{$list[$_]}[0]\n";
    printf "%-16s   %d\n", $list[$_], $Fromstat{$list[$_]}[0];

    for $port (keys(%{$Fromstat{$list[$_]}[1]}))
    {
	@a=sort byval_to (keys(%{$Tomap{"$list[$_]:$port"}}));

	printf "\t%-10s -> %-16s    %d\n", $port, $a[0],
		$Tomap{"$list[$_]:$port"}{$a[0]};
		# $Fromstat{$list[$_]}[1]{$port};
	shift(@a);
	foreach $el (@a)
	{
	    printf "\t%10s -> %-16s    %d\n", "",  $el,
		$Tomap{"$list[$_]:$port"}{$el};
	}
    }
}

@list=sort byval_port (keys(%Portstat));

print "\n" . bold("Top 20 attacked ports:") . "\n";

for $i (0..19)
{
    last if($i > $#list);
    printf "%-16s   %d\n", $list[$i], $Portstat{$list[$i]}[0];

    $sortval=$list[$i];
    @tmplist=sort byval_port2 (keys(%{$Portstat{$list[$i]}[1]}));

    if($#tmplist<5)
    {
	$n=$#tmplist+1;
    }
    else
    {
	$n=5;
    }

    print "\tTop $n attackers on this port:\n";

    for(@tmplist[0..$n-1])
    {
	printf("\t\t\t%-16s  %d\n","$_:",$Portstat{$list[$i]}[1]{$_});
    }
}
print "\n" . bold("Stuff that made no sense") . "\n$huh\n";
print "</pre>\n" if($htmlOutput);
