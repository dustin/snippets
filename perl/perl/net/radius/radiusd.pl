#! /usr/local/bin/perl
#
# Perl radiusd, version 1.6
# By Michael O'Reilly (michael@iinet.net.au)
#   http://www.iinet.net.au/~michael
#
#
#
use RADIUS::Dictionary;
use RADIUS::Packet;
use IO::Socket::INET;
use Net::LDAP;
use Fcntl;
use strict;

use Getopt::Std;
use vars qw($dict @config %secrets %clients $config_time);

use vars qw($opt_x $opt_a $opt_d $opt_l $opt_c $opt_p $opt_e);
use vars qw($opt_w $opt_s $opt_z $rad_timeout $rad_retries);
use vars qw(@vacant_ports @old_args);
use vars qw(%port_list %user_list $children);

use vars qw(%ip_pools);

$opt_x = 0;			# Debugging off by default.
$opt_a = '/var/log/radacct';	# Accounting directory.
$opt_d = '/etc/raddb';		# dbase files.
$opt_l = '';	# Logging file. STDERR by default.
$opt_c = '';			# Local code...
$opt_p = 1645;			# Default port.
$opt_w = undef;			# Port logging ...
$opt_s = 0;			# multi-threaded by default.
$opt_z = '/var/log/radpool';	# User list.
$opt_e = '/home/radius/bin/radiusd.pl';
$children = 0;

@old_args = @ARGV;

getopts('xsa:d:l:c:p:w:z:');	# Check for arguments.


$rad_timeout = 3;		# 3 second time for querying other
				# radius servers.
$rad_retries = 3;		# Try the server up to 3 times.


if (length($opt_l)) {
    open(STDERR, ">>$opt_l") or
	warn("Unable to open log file: $opt_l: $!");
}

if (length($opt_c)) {
    require $opt_c;		# Require local code...
}

read_pool($opt_z) if $opt_z;

#
# Hack! If you have local code you'd like to link it, drop it in this
# file, and it'll get auto-required. I use this to handle our local
# user dbase (uses an external dbase instead of just getpwnam() )
#
if ( -f "$opt_d/radiuslocal.pl") {
    mlog("radiuslocal.pl require.\n");
    require "$opt_d/radiuslocal.pl";
}

#
# set current dir.. "
#

chdir($opt_d);

# Parse the RADIUS dictionary file
$dict = new RADIUS::Dictionary "dictionary"
    or die "Couldn't read dictionary: $!";

read_users('users');

(-f('clients.conf') and read_config('clients.conf')) or
    read_clients('clients');

# Set up the network socket.

print "Port is $opt_p\n";
my $s = IO::Socket::INET->new(
	# 'LocalPort' => $opt_p,
	'LocalAddr' => "0.0.0.0:$opt_p",
	'Proto' => 'udp',
	'Type' => SOCK_DGRAM,
	'Debug' => $opt_x,
	) or die($!);
$s->blocking(0) or die("Couldn't make socket non-blocking: $!");

$opt_p++;
my $a = IO::Socket::INET->new(
	'LocalAddr' => "0.0.0.0:$opt_p",
	'Proto' => 'udp',
	'Debug' => $opt_x
	) or die($!);
$a->blocking(0) or die("Couldn't make socket non-blocking: $!");

#
# Clean zombies..
#
$SIG{'CHLD'} = sub { wait; --$children; };


# Loop forever, recieving packets and replying to them
#
mlog("Radiusd started.\n");
my $loop = 0;
while (1) {
    my ($rec, $whence);

    if ($loop ++ > 5000) {
		# time to re-start...
		exec($opt_e, @old_args) or die("exec: $!");
    }
    if ($config_time != (stat('users'))[9]) {
	read_users('users');
    }

				# Wait for a packet
				# We check for a packet on both the
				# sockets we're listening on.
    my $ein = '';
    vec($ein, $s->fileno, 1) = 1;
    vec($ein, $a->fileno, 1) = 1;

				# Wait indefinately for a packet.
    my $nfound = select($ein, undef, $ein, undef);
				# skip if nothing ready. We probably
				# got a signal.
    next if ($nfound <= 0);


				# Check for a packet on the accounting
				# socket.
    $whence = $a->recv($rec, 1550);
    if( ! length($rec)) {	# Not ready. Try the other socket.

				# Check for a packet on the
				# authentication socket.
    $whence = $s->recv($rec, 1550);
	next if ! length($rec);	# if no packet, wait for another one.
    }

				# Log the packet. Is this too verbose?

	# FIXME
	my $from="localhost";
    # my $from = $s->format_addr($whence, 1); # Ouch. this is very
				# expensive. get protobynum!
				# gethostbyaddr! Even with numeric, it
				# calculates the info, and throws it
				# away!

    mlog("Packet in from $from, len ". length($rec).".\n") if $opt_x;

				# Get the IP address of the packet
				# source.
    my $client = (split(/:/,  $from ))[0];
				# Is it a source we recognise?
    if (! defined $secrets{$client}) {
	mlog("Packet from unknown client $client.\n");
	next;
    }

				# Unpack it
    my $p = new RADIUS::Packet $dict, $rec;

				# If debugging, dump the packet
				# contents to STDOUT
    if($opt_x) {
	print "Packet in!\n";
	$p->dump;
	print "EOP\n";
    }

				# If it's an authentication
				# request...
    if ($p->code eq 'Access-Request') {
	my $pid;
				# If we're not single-threaded, fork
				# off the authentication server.

	if (! $opt_s) {
	    if ($children > 30) { # too many children, run it in-line.
		$pid = -1;
	    } else {
	        $pid = fork;
	    }
	    (++$children, next) if ($pid > 0);
	    alarm(30);		# Maximum of 30 seconds to get a reply
				# out. Murder ourselves if we take
				# longer.
	}

				# Build a return packet.
	my $rp = handle_authentication($p, $client, $secrets{$client});

	# $s->sendto(auth_resp($rp->pack, $secrets{$client}) , $whence);
	$s->send(auth_resp($rp->pack, $secrets{$client}), 0, $whence);
	mlog("Reply sent\n") if $opt_x;
	$rp->dump if $opt_x;

				# If we're multi-threaded, and this is
				# a client, then get out of here.
	exit(0) if ((! $opt_s) and ($pid == 0));


				# Else if it's an accounting
				# request...
    } elsif ($p->code eq 'Accounting-Request') {
				# Make the directory in case it
				# doesn't exist.
	mkdir("$opt_a/$client", 0755);
				# Write the data out to the file. No
				# checking performed, mostly cos I've
				# no idea what we should do if it
				# fails. :)
	open A, ">>$opt_a/$client/detail";
	print A "".localtime(time())."\n";
	foreach ($p->attributes) {
	    print A "\t$_ = ".($p->attr($_))."\n";
	}
	print A "\tTimestamp = ".(0+time())."\n";
	print A "\n";
	close A;

				# Send an acknowledgement back to the
				# client.
	my $rp = new RADIUS::Packet $dict;
	$rp->set_code('Accounting-Response');
				# Carry over the indentifierers from
				# the request.
	$rp->set_identifier($p->identifier);
	$rp->set_authenticator($p->authenticator);

	$s->sendto(auth_resp($rp->pack, $secrets{$client})
		   , $whence);


	my $port = $p->attr('NAS-Port'); # This is dictionary specific
				# apparently.

	$port %= 65536;		# Get rid of the extended port in
				# as5x00's..
	if ($p->attr('Acct-Status-Type') eq 'Start') {
	    run_login($client, $port, $p);
	} elsif ($p->attr('Acct-Status-Type') eq 'Stop') {
	    run_logout($client, $port, $p);

				# handle free'ing up IP address
				# from pools.
	    my $ip =  $p->attr('Framed-IP-Address');
	    if (defined $ip) {
		foreach (keys %ip_pools) {
		    mlog("Trying $ip against $ip_pools{$_}.\n") if $opt_x;
		    $ip =~ /^$ip_pools{$_}$/ or next;
		    put_ip($_, $ip);
		    mlog("$ip restored to $_\n");# if $opt_x;
		    last;
		}
	    }
	}
    } else {
				# It's not an Access-Request, it's not
				# accounting. WTF!?
	print "Unexpected packet type recieved.";
	$p->dump;
    }
}

#
# a login has occured. Do any magic that we feel like doing...
#
sub run_login {
    my($client, $port, $p) = @_;

    mlog("login $client - $port ($children)\n") if $opt_x;

    my $data = "Timestamp = ".
	(0+time())."\n";	# Radius start records don't have
				# a time stamp!! wild!

    foreach ($p->attributes) {
	$data .= "$_ = ".($p->attr($_))."\n";
    }

    if ($opt_w) {
				# note that $client is safe becuase we
				# took it from the packet headers, and
				# $port is safe cos it's the result of
				# math.

	open A, ">$opt_w/$client.$port";
	print A $data;
	close A;
    }

				# Update the user list pool file.

				# Hmm. I think there's a bug here. If
				# the disk fills it's possible that
				# two different entries will be
				# allocated the same slot (stat,
				# failed write, stat, blam!). Clean
				# fix?

    if ($opt_z) {
	my $tag = "$client.$port";
	$user_list{$tag} = $data;
	if (! defined $port_list{$tag}) {
	    my $size = shift @vacant_ports;
	    if (! $size) {
		$size = (stat(POOL))[7];
		$size = ($size + 1023) & (~1023); # Round it up to a 1K
				# boundry. Just in case the file size
				# has been munged.
	    }
	    $port_list{$tag} = $size;
	}
	sysseek(POOL, $port_list{$tag}, 0) ;
	syswrite(POOL, pack('a80a944', $tag, $data), 1024);
    }
}

#
# A logout has occured. Run all the appropriate magic..
#
sub run_logout {
    my($client, $port, $p) = @_;
    mlog("logout $client - $port ($children)\n"); # if $opt_x;
    if ($opt_w) {
	unlink("$opt_w/$client.$port");
    }

				# Clear the entry from the user list
				# pool file.
    if ($opt_z) {
	my $tag = "$client.$port";
	delete $user_list{$tag};
	if (defined $port_list{$tag}) {
	    sysseek(POOL, $port_list{$tag}, 0) and
		syswrite(POOL, pack('a80a944', $tag, ''), 1024);
	}
    }
}

sub read_pool {
    my($file) = @_;
    my($buf, $data, $tag, $where, $entries, $cleaned, $vacant);

    open(POOL, "+<$file") or
	(mlog("Failed to open pool '$file': $!\n") and
	 ($opt_z = undef, 1) and
	 return);

    sysseek(POOL, 0, 0);	# Re-wind to beginning of file

    while (sysread(POOL, $buf, 1024) == 1024) {
	$where = sysseek(POOL, 0, 1) - 1024;
	($tag, $data) = unpack('a80a944', $buf);
	$tag =~ s/\0*$//;
	$data =~ s/\0*$//;
	if (!length($tag)) {
	    push @vacant_ports, ($where);
#	    print "$where vacant\n" if $opt_x;
	    ++$vacant;
	    next;
	}
	if (defined $user_list{$tag}) {
				# Dud duplicate in file!?
	    sysseek(POOL, $where, 0);
	    syswrite(POOL, "\0" x 1024, 1024);
	    push @vacant_ports, ($where);
	    print "$where ($tag) cleaned\n" if $opt_x;
	    ++$cleaned;
	    ++$vacant;
	    next;
	}
	$user_list{$tag} = $data;
	$port_list{$tag} = $where;
	$where += 1024;
	++$entries;
    }
				# grammar.
    $entries ||= 'no';
    $cleaned ||= 'none';
    $vacant ||= 'none';

    mlog("Pool read. $entries entries, $vacant vacant, $cleaned cleaned.\n");
} # '

sub getLDAPRadiusAttrs
{
	my($ldap, $dn)=@_;
	my(%ret, $res, $entry);
	%ret=();

	$res=$ldap->search('base' => $dn, 'scope' => 'base',
		'filter' => '(objectclass=*)',
		'attrs' => ['seeAlso' ,'radiusAttribute']); # '
	if($res->code()==0) {
		my($sa);
		$sa=undef;
		# there's only one entry here
		for $entry ($res->all_entries()) {
			my(@a)=$entry->get('radiusAttribute');

			for(@a) {
				my(@b)=split(/\s*=\s*/, $_, 2);
				$ret{$b[0]}=$b[1];
			}
			@a=$entry->get('seeAlso');
			if(@a) {
				$sa=$a[0];
			}
		}
		if(defined($sa)) {
			my(%tmp)=getLDAPRadiusAttrs($ldap, $sa);
			# Populate with the stuff from the seeAlso
			for(keys(%tmp)) {
				if(!defined($ret{$_})) {
					$ret{$_}=$tmp{$_};
				}
			}
		}
	}
	return(%ret);
}

sub digInLDAP
{
	my($u, $p)=@_;
	my($ldap, $dn, $res, $ret, $entry, $filter);

	undef($ret);
	$dn="";

	$ldap=Net::LDAP->new("ldap") || return(undef);

	# We do the search and just return the uid because we want the DN to
	# attempt a binding to and then we'll grab the attributes.
	$filter="(&(objectClass=beyondRadiusEntry)(uid=$u))";
	print "Filter is $filter\n" if($opt_x);
	$res=$ldap->search('base' => 'dc=beyond,dc=com', 'scope' => 'sub',
		'filter' => $filter, 'attrs' => [ 'uid' ]); # '
	if($res->code()==0) {
		# This will get us the last DN...hopefully there'll only be one.
		for $entry ($res->all_entries()) {
			$dn=$entry->dn();
			print "Got dn:  $dn\n" if($opt_x);
		}
	}

	# Only do this if we got a dn
	if($dn ne "") {
		$res=$ldap->bind($dn, $p);
		# If we succesfully bound, let's look up the attributes.
		if($res->code()==0) {

			# This is where we put stuff.
			my(%ret);

			%ret=getLDAPRadiusAttrs($ldap, $dn);
			if(%ret) {
				$ret=\%ret;
			}
		}
	}

	return($ret);
}

#
# Run thru the configuration file, working out
# what to say, and building a packet to say it..
# '
sub run_config {
    my($name, $pass, $attrs, $p, $client) = @_;
    my($rp, $server) = undef;

    $name =~ s/[\r\n\t ]*$//;	# kill trailing whitespace in username..
    $pass =~ s/[\r\n\t ]*$//;	# kill trailing whitespace in
				# passwd. This shouldn't happen.
				# but it does.

				# For each entry in the config file..

	# Get our response packet
	(ref $rp) or ($rp = new RADIUS::Packet $dict);

	# Carry over the indentifierers from
	# the request.
	$rp->set_identifier($p->identifier);
	$rp->set_authenticator($p->authenticator);

	my($ret);
	eval {
		$ret=digInLDAP($name, $pass);
	};

	# Was there a problem with the LDAP search?  NAK the user.
	if($@) {
		mlog("Error in LDAP stuff:  $@\n");
		undef($ret);
	}

	if(defined($ret)) {
		# Set the attributes for the reply..
		my %a = %{$ret};

		map {
		$rp->set_attr($_, $a{$_});
		} keys %a;
		$rp->set_code('Access-Accept'); # '
	} else {
		mlog("User $name not matched on $attrs->{'Called-Station-Id'} with "
			. "\"$pass\" calling from $attrs->{'Calling-Station-Id'}\n"); # '
		$rp->set_code('Access-Reject'); # '
	$rp->set_identifier($p->identifier);
	$rp->set_authenticator($p->authenticator);
	}

	return($rp);
}

sub handle_authentication {
    my ($p, $client, $secret) = @_;

    my $pass = $p->password($secret);
    $pass =~ s/\000.*$//;

    my %attrs = ();
    map { $attrs{$_} = $p->attr($_); } $p->attributes;

    my $name = lc($p->attr('User-Name'));
#    if ($p->attr('Called-Station-Id') eq '92780990') {
#	$name .= '@ois.net.au';
#    }
    $name =~ s/\s+$//;		# Delete trailing whitespace...
    $pass =~ s/\000.*$//s;
    return run_config($name, $pass, \%attrs, $p, $client);
}


sub read_users {
    my($file) = @_;
    open CONFIG, $file or die("Unable to read config file");
    $config_time = (stat(CONFIG))[9];

    my $line = 0;
    my $errors;
    my @newconfig;

    LINE: while (<CONFIG>) {
	++$line;
	s/\s*#[^"]*$//;		# Strip comments.
	s/\s+$//;		# Strip trailing whitespace.
	/^$/ and next;		# skip blank lines.

	/^([^\s]+)\s+(.*)$/ or
	    mlog("Invalid user line $line: $_") and ++$errors and next;

	my($user, $cond) = ($1, $2);
	my(@c, %attrs);

				# Hairy code. We need to deal with the
				# possibility of commas inside quoted
				# strings. There is almost certainly
				# a better way of doing this...

	$_ = $cond;
	while ($cond =~ /=/) {
	    $cond =~ s/^([^\s=,]+)\s*=\s*"([^"]*)"\s*,\s*(.*?)$/$3/
                                            or
	    $cond =~ s/^([^\s=,]+)\s*=\s*([^"\s,]+)\s*,\s*(.*?)$/$3/
                                            or
            $cond =~ s/^([^\s=,]+)\s*=\s*"([^"]*)"\s*$//
                                            or
	    $cond =~ s/^([^\s=,]+)\s*=\s*([^\s",]*)\s*$//
                                            or
            mlog("Invalid condition (line $line): $cond\n") and
                                            ++$errors and last;
	    push @c, ($1, $2);
        }

	while (<CONFIG>) {
	    /^\s+/ or last;
	    ++$line;

	    s/\s*#[^"]*$//;		# Strip comments.
	    s/\s+$//;		        # Strip trailing whitespace.
	    /^$/ and next;		# skip blank lines.

	    /^\s+([^\s=]+)\s*=\s*"(.+?)"\s*,?\s*$/ or
	    /^\s+([^\s=]+)\s*=\s*(.+?)\s*,?\s*$/ or
		mlog("Invalid attribute (line $line): $_") and
		    ++$errors and next;
	    $attrs{$1} = $2;

			# hackery to handle initialization of IP pools.
	    my($lhs, $rhs) = ($1, $2);
	    if (lc($lhs) eq 'framed-ip-address' and $rhs =~ /\//) {
		my $regex = ippool_getregex($rhs);
		if (!defined $regex) {
		    mlog("Invalid IP pool attempted. $rhs -> $regex\n");
		} else {
		    $ip_pools{$rhs} = $regex;
		    mlog("Found IP pool: $rhs -> $regex\n");
		}
	    }
	}
				# And save the entry to
	push @newconfig, ( [ $user, \@c, \%attrs ] );
	redo LINE unless eof;
    }
    close(CONFIG);
				# If there are errors, and this is a
				# reload, then ignore the file.
    if ($errors and $#config >= 0) {
	mlog("Errors in users file. Not loading.\n");
	return;
    }
				# Everything ok, install the new
				# configuration.
    @config = @newconfig;
}

#
# Read the client secrets file.
#
sub read_clients {
    my($file) = @_;
    open C, $file or
	warn("Unable to read '$file' file: $!") and
	    return;

    my($line) = 0;

    while (<C>) {
	++$line;
	s/\s*#[^"]*$//;		# Strip comments.
	s/\s+$//;		# Strip trailing whitespace.
	/^$/ and next;		# skip blank lines.

	my @a = split;
	if ($#a < 1) {
	    warn("Invalid line $line in client file\n");
	    next;
	}
	$secrets{$a[0]} = $a[1];	# Ignore the short name for now.
    }
}

#
# Read the config file.
#
sub read_config {
    my($file) = @_;
    open CONFIG, $file or
	warn("Unable to read '$file' file: $!") and
	    return;

    my($line) = 0;

  LINE: while (<CONFIG>) {
      ++$line;
      s/\s*#[^"]*$//;		# Strip comments.
      s/\s+$//;		# Strip trailing whitespace.
      /^$/ and next;		# skip blank lines.

      /^([^\s]+)\s+(.*)$/ or
	  mlog("Invalid config line $line: $_") and next;

      $secrets{$1} = $2;
      my $client = $1;
      $clients{$client} = {};
      while (<CONFIG>) {
	  /^\s+/ or last;
	  ++$line;

	  s/\s*#[^"]*$//;		# Strip comments.
	  s/\s+$//;		        # Strip trailing whitespace.
	  /^$/ and next;		# skip blank lines.

	  /^\s+([^\s=]+)\s*=\s*"(.+?)"\s*,?\s*$/ or
	      /^\s+([^\s=]+)\s*=\s*(.+?)\s*,?\s*$/ or
		  mlog("Invalid client attribute (line $line): $_") and
		      next;
	  $clients{$client}->{$1} = $2;
	}
      redo LINE unless eof;
    }
    close(CONFIG);
}


#
# Ask a remote-radius server for authentication...
# Given a server to use, the secret for that server, and a
# template packet..
#
# returns undef for a fail, else the reply packet is returned..
#

sub radius_is_valid {
    my($server, $secret, $p, $pass) = @_;

    my $s = new Net::UDP or die($!);
    $s->connect($server, 1645) or die("Couldn't bind: $!");
    $s->fcntl(F_SETFL, $s->fcntl(F_GETFL,0) | O_NONBLOCK)
	or die("Couldn't make socket non-blocking: $!");

    my $rp = new RADIUS::Packet $dict;
    $rp->set_code('Access-Request');
    $rp->set_identifier($p->identifier);
    $rp->set_authenticator($p->authenticator);

    foreach ($p->attributes) {
#		next if !length($p->attr($_));
#		print "$_: '".$p->attr($_)."'\n";
	$rp->set_attr($_, $p->attr($_));
    }

    $rp->set_attr('Password', $pass);
    $rp->set_attr('Password', $rp->password($secret));

    my $nfound;
    for (my $i = 0; $i < $rad_retries; ++$i) {
	$s->send($rp->pack);

	$nfound = $s->select(1, 0, 1, $rad_timeout);

	last if $nfound > 0;
    }

    mlog("Timed out querying server $server"), return undef
	if $nfound <= 0;	# timeout

    # Get the data
    my($whence, $rec);

    $whence = $s->recv($rec, 1550);
    return undef if ! length($rec);

    $p = new RADIUS::Packet $dict, $rec;

    print "Reply Packet in!\n" if $opt_x;
    $p->dump if $opt_x;

    return $p if ($p->code eq 'Access-Accept');
    return undef;
}

#
# Log a line somewhere.
#
sub mlog {
    print STDERR "".localtime()." " . join('', @_ );
    return 1;
}

#
# Return true if the username matches the password.
#

sub system_is_valid {
    return &local_is_valid(@_)
	if (defined(&local_is_valid));
    print "Using system auth for $_[0]\n" if $opt_x;
    my($name, $pass) = @_;
    my($n, $crypt) = getpwnam($name);
    return undef if ($n ne $name);
    $pass = crypt($pass, $crypt);
    return undef if ($pass ne $crypt);
    return 1;
}


sub get_ip {
        my($pool, $data) = @_;

        open IPPOOL, "+<$pool" or return undef;
        flock(IPPOOL, 2);               # get an exclusive lock on the file.
        (read(IPPOOL, $data, 80) == 80) or return undef;
        my($start, $size, $regex) = split(/,/, $data);
        while (read(IPPOOL, $data, 20) == 20) {
                $data =~ s/^F/./ or next;
                my $pos = tell(IPPOOL) - 20;
                seek(IPPOOL, $pos , 0);
                print(IPPOOL $data) or print("write failed: $!\n");
                close(IPPOOL) or print("close failed: $!\n");
                                # flush data, and release lock.
                $data =~ s/^.//;
                $data =~ s/\s+//g;
                return $data;
        }
        close IPPOOL;
        return undef;
}

sub put_ip {
        my($pool, $ip, $data) = @_;

        open IPPOOL, "+<$pool" or return undef;
        flock(IPPOOL, 2);               # get an exclusive lock on the file.
        (read(IPPOOL, $data, 80) == 80) or return undef;
        my($start, $size, $regex) = split(/,/, $data);
        while (read(IPPOOL, $data, 20) == 20) {
                $data =~ s/^.$ip /F$ip / or next;
                my $pos = tell(IPPOOL);
                seek(IPPOOL, $pos - 20, 0);
                print(IPPOOL $data);
                close IPPOOL;   # flush data, and release lock.
                $data =~ s/^.//;
                $data =~ s/\s+//g;
                return $data;
        }
        close IPPOOL;
        return undef;
}

sub ippool_getregex {
	my($pool, $data) = @_;

        open IPPOOL, "+<$pool" or return undef;
        (read(IPPOOL, $data, 80) == 80) or return undef;
        my($start, $size, $regex) = split(/,/, $data);
	$regex =~ s/\s+//gs;
	return $regex;
}
