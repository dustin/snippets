# This is a library type thing of all the routines to grab stat data.
# $Id: statlib.pl,v 1.3 1997/12/12 21:15:18 dustin Exp $

# this reads in a file ignoring lines starting with # and empty lines

require '/home/monitor/lib/statlib.conf';

# A damned global.
@alrmDecoded;

# Library thing to get date info.

sub timeStamps
{
    local(@a);

    @a=localtime();

    $a[0]="0$a[0]" if($a[0]<10);
    $a[1]="0$a[1]" if($a[1]<10);
    $a[2]="0$a[2]" if($a[2]<10);

    $a[4]++;
    $a[3]="0$a[3]" if($a[3]<10);
    $a[4]="0$a[4]" if($a[4]<10);
    $a[5]+=1900;

    return("$a[2].$a[1].$a[0]", "$a[4].$a[3].$a[5]");
}

sub ensurepath
{
    local($fn)=@_;
    local(@a, $np);

    if(-d $fn)
    {
        return();
    }

    @a=split(/\//, $fn);

    pop(@a);
    $np=join('/', @a);
    &ensurepath($np);

    if(! -d $np )
    {
        mkdir($np, 0755);
    }
}

sub readcommented
{
    my($file)=@_;
    my(@a, $recback);

    $recback=$/;
    $/="\n";

    open(RCIN, $file);
    while(<RCIN>)
    {
        next if(/^#/);
        next if(!/[A-z0-9]/);

        chop($_);
        push(@a, $_);
    }
    close(RCIN);
    $/=$recback;
    return(@a);
}

sub bydisk
{
    my(@a, @b);

    @a=split(/s+/, $a);
    @b=split(/s+/, $b);

    if($a eq $b)
    {
        return($a[1] cmp $b[1]);
    }
    else
    {
        return($a cmp $b);
    }
}

# this returns a list of lists of all the entries to be done

sub list_diskinfos
{
    my($oldthing, $machine, $n);
    my(@lol, @tmp, @tmp2, @a);

    @a=readcommented($diskScanFile);

    $oldthing=".";
    $n=0;
    @tmp2=();
    @tmp3=();

    foreach (sort bydisk (@a))
    {
        @tmp=split;
        if($oldthing eq $tmp[0])
        {
            push(@tmp2, [ @tmp ]);
        }
        else
        {
            if($oldthing ne ".")
            {
                push(@lol, [ @tmp2 ]);
            }
            @tmp2=();
            push(@tmp2, [ @tmp ]);
        }
        $oldthing=$tmp[0];
    }

    if($oldthing eq $tmp2[0][0])
    {
        push(@lol, [ @tmp2 ]);
    }
    else
    {
        push(@tmp2, [ @tmp ]);
        push(@lol, [ @tmp2 ]);
    }

    return(@lol);
}

# This is just in case someone is wondering how to display the return
# value of the last function.

sub display3dlol
{
    my(@lol)=(@_);
    my($i, $j, $k);

    print "Commensing display...\n";
    # This is how one might go about looking at that list...
    #
    for $i ( 0 .. $#lol ) {
        for $j ( 0 .. $#{$lol[$i]} ) {
            for $k ( 0 .. $#{$lol[$i][$j]} ) {
                print "$i $j $k is $lol[$i][$j][$k]\n";
            }
        }
    }
}

# alarm optimization code, long and drawn out...

sub decodeAlarm
{
    my($recipient, @alrm)=@_;
    my($alarms, $alarm, @out);

    if(defined($alrm[1]{$recipient}))
    {
        foreach $alarm (@{$alrm[1]{$recipient}})
        {
            if($alarm ne $recipient)
            {
                push(@alrmDecoded, decodeAlarm($alarm, @alrm));
            }
        }
    }
    else
    {
        if(defined($alrm[0]{$recipient}))
        {
            foreach $alarm (@{$alrm[0]{$recipient}})
            {
		if(defined($alrm[0]{$recipient}))
		{
                    push(@alrmDecoded, $alarm);
		}
		else
		{
                    push(@out, $alarm);
		}
            }
        }
	else
	{
	    print "Ain't got no $recipient\n";
	}
    }

    return(@out);
}

sub makeOptimalAlarmList
{
    my(@in)=@_;
    my(%h, @pages, @email, %doms, @ret, $tmp);

    # get rid of duplicates

    @pages=@email=@ret=%doms=();

    foreach(@in)
    {
	$h{$_}=1;
    }

    foreach(keys(%h))
    {
	if(/P:(.*)/)
	{
            push(@pages, $1);
	}

	if(/E:(.*)\@(.*)/)
	{
	    push(@{$doms{$2}}, $1);
	}
    }

    foreach(keys(%doms))
    {
	push(@email, "E:" . (join(',',@{$doms{$_}}) . "\@$_") );
    }

    if($#pages>0)
    {
	push(@ret, "P:" . join(',', @pages));
    }

    push(@ret, @email);

    return(@ret);
}

sub optimalAlarms
{
    my($key, @mylist, $tmp, $n, @deletes);
    my(@hs);

    @hs=readInAlarms();

    foreach $key (keys(%{$hs[1]}))
    {
        foreach $tmp (@{ $hs[1]{$key} })
        {
            @alrmDecoded=();
            decodeAlarm($tmp, @hs);
            push(@mylist, @alrmDecoded);
        }

        $hs[0]{$key}=[makeOptimalAlarmList(@mylist)];
        @mylist=();
	# Drop the class.
        push(@deletes, $key);
    }
    foreach(@deletes)
    {
        delete($hs[1]{$_});
    }
    return(@hs);
}

# This is to read in the alarm lists

sub readInAlarms
{
    my(@a);
    my(@loh, $key, $c, $tmp);

    $c=0;

    for(readcommented($alarmsList))
    {
       @a=split(/\s+/, $_);
       $key=shift(@a);

       if(/^CLASSES/)
       {
           $c=1;
           next;
       }

       $loh[$c]{$key}=[@a];
    }

    return(@loh);
}

sub sendmail
{
    my($to, $subject, $message)=@_;
    my(@to, $domain, $tmp);

    ($to, $domain)=split(/\@/, $to);
    @to=split(/,/, $to);

    @to=map("$_\@$domain", @to);
    $to=join(", ", @to);

    open(MAIL, "|/usr/lib/sendmail -oi -t");

    print MAIL "From:  Dustin's Alarm thing <dustin\@cybersource.com>\n";
    print MAIL "To: $to\n";
    print MAIL "Subject: $subject\n\n";
    print MAIL $message;

    close(MAIL);
}

sub sendpage
{
    my($message, @to);
    $message=shift(@_);
    @to=@_;

    $message=~s/[\n\r]+/ /g;

    openhost($pageServer, $snppPort);

    foreach(@to)
    {
	print S "page $_\n";
    }

    print S "message $message\n";

    print S "priority high\n";

    print S "send\nquit\n";

    sleep(2);

    close(S);
}

sub checkCutoff
{
    my(@stats);
    my($time);

    $time=time();
    @stats=stat($CUTOFFLOG);

    # this will cause alarms to be ignored if the file has been modified
    # less than $cutoffTimeout seconds ago

    return( ($time-$stats[9]) < $cutoffTimeout);
}

sub doalarm
{
    my($alarm, $message)=@_;
    my(@a);

    return if(&checkCutoff);

    @a=split(/:/, $alarm);

    if($a[0] eq "E")
    {
        sendmail($a[1], "ALARM", $message);
    }

    if($a[0] eq "P")
    {
        sendpage($message, split(/,/, $a[1]));
    }
}

sub doalarms
{
    my($recipient, $message)=@_;
    my(@alrm, @alarms, $alarm);

    @alrm=optimalAlarms();

    # try class first
    if(defined($alrm[1]{$recipient}))
    {
        foreach $alarm (@{$alrm[1]{$recipient}})
        {
            if($alarm ne $recipient)
            {
                doalarms($alarm, $message);
            }
        }
    }
    else
    {
        if(defined($alrm[0]{$recipient}))
        {
            foreach $alarm (@{$alrm[0]{$recipient}})
            {
                doalarm($alarm, $message);
            }
        }
        else
        {
            sendmail("dustin\@cybersource.com", "alarm problem",
                    "no alarm user $recipient");
        }
    }
}

# log stuff

sub readInLogstuff
{
    my(%ht, @a, $key, @val, $n);

    @a=readcommented($logScanFile);

    for($n=0 ; $n < $#a; $n+=3)
    {
        $key=$a[$n];
        @val=( $a[$n+1], $a[$n+2] );

        $ht{$key}=[ @val ];
    }

    return(%ht);
}

sub trylist
{
    my($line, %list)=@_;
    my($key, @val, $val, %result);

    while( ($key, @val)=each(%list))
    {
        $val=$val[0][0];
        if($line=~/$val/)
        {
            if(defined($result{$key}))
            {
                $result{$key}++;
            }
            else
            {
                $result{$key}=1;
            }
        }
    }
    return(%result);
}

sub scanLogFile
{
    my($fn)=@_;
    my(%list, %res, %r, $key, %db, $date, $stamp);

    $stamp=0;

    %list=readInLogstuff();

    open(LOGFILE, $fn);

    if($stamp)
    {
        dbmopen(%db, "/tmp/logscanstamp", 0644);

        if(defined($db{$fn}))
        {
            while(<LOGFILE>)
            {
                last if(/$db{$fn}/);
            }
        }
    }

    while(<LOGFILE>)
    {
        $_=~/([A-z]{3}\s+[0-9]+\s+[0-9\:]+)/;
        $date=$1;

        %r=trylist($_, %list);
        for $key (keys(%r))
        {
            if(defined($res{$key}))
            {
                $res{$key}[0]+=$r{$key};
            }
            else
            {
                $res{$key}[0]=1;
            }

	    $res{$key}[1]=$date;
        }
    }
    close(LOGFILE);

    if($stamp)
    {
        $db{$fn}=$date;
        dbmclose(%db);
    }

    return(%res);
}

sub timeout
{
        print "Connection timed out.\n";
        return;
}

sub pslistInitPartial
{
    my(@hosts)=@_;
    my($host, %ret, $size, $input, @a, @b);
    my($regex, $i);

    $regex="(.{8})(.{6})(.{6})(.{3})(.{9})(.{9})(.{5})(.*)";

    foreach $host (@hosts)
    {
	&openhost($host, 6013);
	print S "ps -ef\r\n";

        <S>;

	while(<S>)
	{
            last if(/^EOF/);
	    next unless(/$regex/);

	    @a=($1, $2, $3, $4, $5, $6, $7, $8);

	    foreach $i (0..$#a)
	    {
		$a[$i]=~s/\s*//;
	    }

	    push( @{$ret{$host}}, [@a]);
	}

        print S "quit \r\n";
        close(S);
    }

    return(%ret);
}

sub pslistInit
{
    pslistInitPartial(&readcommented($allMachinesFile));
}

sub appmonInit
{
    my($host, %ret, @hosts, $size, $input, @a, @b);

    @hosts=&readcommented($allMachinesFile);

    foreach $host (@hosts)
    {
        &openhost($host, 6013);

	print S "quickstat\r\n";
	chop($size=<S>);

	read(S, $input, $size);

	@a=split(/\n/, $input);

	foreach(@a)
	{
	    next if(/^#/);
	    next if(!/[A-z0-9]/);
	    @b=split(/:/, $_, 2);

	    $ret{$host}{$b[0]}=$b[1];
	}

	print S "quit\r\n";
	close(S);
    }

    return(%ret);
}

sub openhost
{
local ($remote, $portnum)=@_;
local ($port, $name, $aliases, $proto, $type, $len);
local ($sockaddr, $hostname);

        $sockaddr='S n a4 x8';
        $hostname="0.0.0.0";

        $SIG{'ALRM'}= 'timeout';
        ($name, $aliases, $proto)=getprotobyname('tcp');
        ($name, $aliases, $port)=getservbyname($portnum, 'tcp');

        ($name, $aliases, $type, $len, $thisaddr)=gethostbyname($hostname);
        ($name, $aliases, $type, $len, $thataddr)=gethostbyname($remote);
        if($name eq "")
        {
                print "$ARGV[0] Unable to resolve hostname $remote.\n";
                return;
        }
        $me=pack($sockaddr, 2, 0, $thisaddr);
        $rem=pack($sockaddr, 2, $portnum, $thataddr);

        if(!socket(S, 2, 2, $proto))
        {
                print "$ARGV[0] Can't open socket: $!\n";
                return;
        }

        if(!bind(S, $me))
        {
                print "$ARGV[0] Can't bind socket: $!\n";
                return;
        }

        if(!connect(S, $rem))
        {
                print "$ARGV[0] Can't connect to $remote: $!\n";
                return;
        }

        select(S); $|=1; select(stdout); $|=1;
}

1;
