# Generic database object.
#
# $Id: DB.pm,v 1.2 2000/05/27 08:28:34 dustin Exp $

=pod

=head1 NAME

Bynd::DB - Database handler/query routines.

=head1 SYNOPSIS

  use Bynd::DB;

  $dbh=Bynd::DB->new;

  $s=$dbh->doQuery($query);

  [...]

=head1 EXAMPLE
	(just one of the many many many ways to use this)

	# $d is the database object
	# $s is a result object
	# $r is a row of data (as an array reference -- Data::Dumper will show this)

	use Bynd::DB;
	use Data::Dumper;

	# Below specifies everything, if nothing is specified to the new function
	# $ENV{DSQUERY} and $ENV{REMOTE_USER} are used to connect to the database.
	$d=Bynd::DB->new ("dbserver" => "SWNET_DEV", "dbuser" => "somelogin",
						"dbpass" => "somepassword", "script" => "myScript.pl");

	# The eval block allows a trapping on a die within the query
	eval {
		$s=$d->doQuery("select * from store");
		while ($r=$s->fetch)
		{
			print Dumper ($r);
		}
		$s->finish;
	};

	# If the query dies then this will die the app with the DB error.
	if ($@) {
		die "Problem executing query with [$@]\n";
	}



	#
	# For a multiple result query do the follwing to your eval:
	# NOTE: This is not a function of Bynd::DB, but is a function
	#       of DBD::Sybase.
	eval {
		$s=$d->doQuery("select * from store");
		do {
			while ($r=$s->fetch)
			{
				print Dumper ($r);
			}
		} while($s->{syb_more_results});
		$s->finish;
	};




=head1 DESCRIPTION

This object is used as a custom wrapper for DBI objects, it basically does
all the smart stuff we need to do database work here.

=head1 DEADLOCKS

Deadlock handling in here is very minimal.  It works like this:

	If this is the first query in a given transaction, the query will be
attempted up to three times and then doQuery will die (to be caught in an
eval).

	If this is not the first query in a transaction, doQuery will
immediately die.

	The reason we don't reattempt failed queries in the middle of a
transaction is because the retry also closes the database connection and
opens a new one.

=head1 FUNCTIONS

=over 4

=cut

use DBI;
use DCache;
use Fcntl();
use SPY::TmpFile;
use strict;


{
	package SPY::DB;

	use Data::Dumper;
	use File::Basename;

	# Call count
	use vars qw($COUNT);
	$COUNT=0;


=pod

=item *
new

Create a new instance of a Bynd::DB object.  You can pass tagged options in,
too, including the following:

 * dbserver - Name of the server to connect to
 * dbuser - Username to connect to the database as
 * dbpass - password for dbuser
 * script - name of script that is running
 * verbose - Print out queries as HTML comments.
 * interfaces - a full directory path to a specific interfaces file

=cut

	sub new {
		my $proto=shift;
		my $class = ref($proto) || $proto;
		my (%options) = @_;
		my $self = {
            options    => \%options,
            querycount => 0,
			'ac' => 0,
			'notbaderr' => ""
        };

		bless($self, $class);
		return($self);
	}

=pod

=item *
disconnect

If the Bynd::DB object has a database connection, this will close it
explicitly, else you can wait for the destructor to be called.

=cut

	sub disconnect {
		my($self)=shift;
		if(defined($self->{'dbh'})) {
			$self->{'dbh'}->disconnect();
			if ($self->{debugLog}) {
				$self->{debugLog}->log("disconnect");
			}
		}
	}

	sub DESTROY {
		my($self)=@_;

		if ($self->{debugLog}) {
			$self->{debugLog}->log("DESTROY");
		}

		if(defined($self->{'statement'})) {
			# These evals are for protection only.  We are attempting to
			# cancel and finish statements, but if it doesn't work, we at
			# least tried.  There are lots of valid reasons for these to
			# fail.  On error, there's nothing to do.
			eval {
				$self->{'statement'}->cancel
					|| $self->{debugLog}->log("Cancel failed!!!");
			};
			eval {
				$self->{'statement'}->finish
					|| $self->{debugLog}->log("Finish failed!!!");
			};
		}
		# sleep(1);
		$self->disconnect();
	}

	sub openDB {
		my($self)=shift;
		my($dbname, $dbhost, $dbuser, $dbpass);

        return if ($self->{'dbh'});

		# override user
		if(defined($self->{'options'}{'dbuser'})) {
			$dbuser=$self->{'options'}{'dbuser'};
		}

		if(defined($self->{'options'}{'dbpass'})) {
			$dbpass=$self->{'options'}{'dbpass'};
		}

		if(defined($self->{'options'}{'dbname'})) {
			$dbname=$self->{'options'}{'dbname'};
		}

		if(defined($self->{'options'}{'dbhost'})) {
			$dbhost=$self->{'options'}{'dbhost'};
		}

		if ($self->{debugLog}) {
			$self->{debugLog}->log("connect");
		}

		$self->{'dbh'}=DBI->connect(
			"dbi:Pg:dbname=$dbname;host=$dbhost",
			$dbuser, $dbpass) || die("Connect: $DBI::errstr\n");
		$self->{'dbh'}->{'AutoCommit'}=$self->{'ac'};
	}

=pod

=item *
commit

Commit pending database work.

=cut

	sub commit {
		my($self)=shift;
		if(defined($self->{'dbh'})) {
			$self->{'dbh'}->commit;
			$self->{'querycount'}=0;
		}
	}

=pod

=item *
rollback

Rollback pending database work.

=cut

	sub rollback {
		my($self)=shift;
		if(defined($self->{'dbh'})) {
			$self->{'dbh'}->rollback;
			$self->{'querycount'}=0;
		}
	}

=pod

=item *
hashQuery(I<$query>, I<%options>)

Do a query and return a hash of arrays of the results.

This does so via I<doQuery> below, so all the same rules apply.

The following options are supported:

 cachetime - Amount of time (in seconds) this query is valid.

=cut

	sub hashQuery {
		my($self)=shift;
		my($query, %options)=@_;
		my($r, $s, %ret, $c);

		if(defined($options{'cachetime'})) {
			$c=DCache->new;

			# I needed to have a tmp alternate cachedir for testing.
			# $c->cachedir('/tmp/dustinscache');


			if($c->checkcache($query, $options{'cachetime'})) {
				my($str, $VAR1);
				$str=$c->getcache_only($query);
				# Make sure it has VAR1 in it.
				if($str=~/VAR1/) {
					eval $str;
					# Make sure the eval worked, set $VAR1 to a reference,
					# and that it's a hash reference before returning a
					# dereferenced hash reference here.
					if(!$@ && (ref($VAR1) && ref($VAR1) eq "HASH") ) {
						return(%{$VAR1});
					} # if the cache is valid
				} # if the cache looks like real cache
			} # If the cache time is valid
		} # If we're caching at all

		# This code is reached if the cache is old, invalid, doesn't make
		# sense, won't pass an eval, or sets $VAR1 to something other than
		# a hash reference.

		$s=$self->doQuery($query);
		# print "Got type:  " . $s->{'syb_result_type'} . "\n";
		while($r=$s->fetchrow_hashref) {
			my(%tmp, $k, $v);
			%tmp=%{$r};
			while( ($k,$v)=each(%tmp)) {
				if($self->{'options'}{'verbose'}>2) {
					print "\t<!-- $k=$v -->\n";
				}
				push(@{$ret{$k}}, $v);
			}
		}
		if(defined($options{'cachetime'})) {
			# Data::Dumper has some kinda bug in it.
			$Data::Dumper::Indent=2;
			$Data::Dumper::Purity=0;
			$Data::Dumper::Pad="";
			$Data::Dumper::Varname="VAR";
			$Data::Dumper::Useqq=0;
			$Data::Dumper::Terse=0;
			$Data::Dumper::Freezer="";
			$Data::Dumper::Toaster="";
			$Data::Dumper::Deepcopy=0;
			$Data::Dumper::Quotekeys=1;
			$c->cache($query, 'hashQuery', Dumper(\%ret));
		}
		return(%ret);
	}

=pod

=item *
arrayQuery(I<$query>, I<%options>)

Do a query and return an array of arrays of the results.

This does so via I<doQuery> below, so all the same rules apply.

The following options are supported:

 cachetime - Amount of time (in seconds) this query is valid.

=cut

	sub arrayQuery {
		my($self)=shift;
		my($query, %options)=@_;
		my($r, $s, @ret, $c, %ret);

		if(defined($options{'cachetime'})) {
			$c=DCache->new;

			# I needed to have a tmp alternate cachedir for testing.
			# $c->cachedir('/tmp/dustinscache');

			if($c->checkcache($query, $options{'cachetime'})) {
				my($str, $VAR1);
				$str=$c->getcache_only($query);
				eval $str;
				return(@{$VAR1});
			}
		}

		$s=$self->doQuery($query);
        my $row;
		while($r=$s->fetchrow_arrayref) {
			my(@tmp, $k, $v);
            $row++;
			@tmp=@{$r};
            for $k (0..$#tmp) {
                $v = $tmp[$k];
				if($self->{'options'}{'verbose'}>2) {
					print "\t<!-- column $k row $row=$v -->\n";
				}
				push(@{$ret[$k]}, $v);
			}
		}
		if(defined($options{'cachetime'})) {
			$c->cache($query, 'arrayQuery', Dumper(\@ret));
		}
		return(@ret);
	}

=pod

=item *
doQuery(I<$query>, I<%options>)

Perform a SQL query, will connect to a database if necessary, returns a DBI
query result handler thing.

B<WARNING>!:  This routine dies on failure, if you do not want this, call
it inside a block eval.

=cut

	sub doQuery {
		my $self=shift;
		my($query, %options)=@_;
		my($s,$dbh, $done, $attempt, $error, $maxattempts, $attemptPause);

		# default
		$maxattempts=15;
		$attemptPause=1;
		$attempt=0;
		$done=0;

		# OK, we can open it now
		$self->openDB;


		$done=$attempt=0;

		if($self->{'options'}{'verbose'}) {
			print "<!-- Doing query: $query\nCount is $self->{'querycount'}\n".
				"Retries is $maxattempts\n-->\n";
		}

		$self->{'deadlockRetries'} = 0;
		while($done != 1 && $attempt<$maxattempts) {
			$attempt++;
			# warn "Done = $done / Attempt = $attempt / Max Attempts = $maxattempts";
			# warn "Start loop";
			eval {
				# warn "Start Eval";
				$self->openDB;
				if(defined($self->{'statement'})) {
					undef($self->{'statement'});
				}
				$s=$self->{'dbh'}->prepare($query);
				$self->{'statement'}=$s;
				$self->{'_err'}=$DBI::err;
				$self->{'_errstr'}=$DBI::errstr;
				$self->{'_state'}=$DBI::state;
				$self->{'_query'}=$query;
				if(!$s) {
					$error=$DBI::errstr;
					# warn("Prepare error:  $error");
					$self->{'dbh'}->disconnect;
					undef($self->{'dbh'});
					sleep(1);
					die($error);
				}

				# Stupid ass stderr shit.
				#
				my $tmpfilename=SPY::TmpFile->new_name;
				open(__DB_SAVESTDERR, ">&STDERR");
				# Open this manually, I really want it right...
				sysopen(STDERR, $tmpfilename,
					Fcntl::O_CREAT()|Fcntl::O_RDWR()|Fcntl::O_EXCL(),
					0600);
				$s->execute;
				open(STDERR, ">&__DB_SAVESTDERR");
				open(TMP, "<$tmpfilename");
				$self->{'stderr'}=join('', <TMP>);
				close(TMP);
				unlink("$tmpfilename");

				if($self->{'printstderr'}) {
					print STDERR $self->{'stderr'};
				}

				$self->{'_err'}=$DBI::err;
				$self->{'_errstr'}=$DBI::errstr;
				$self->{'_state'}=$DBI::state;
				if (defined($DBI::err) && $DBI::err == 1205) {
					# warn "Deadlocked!!";
					$self->{'deadlockRetries'}++;
					sleep ($attemptPause++);
					# warn "Done = $done / Attempt = $attempt / Max Attempts = $maxattempts";
				} elsif (!$s || ($DBI::err && !$self->notbaderr($DBI::err)) ) {
					if($^W) {
						warn "DB ERROR!!  $DBI::errstr";
					}
					$error=$DBI::errstr;
					if($s) {
						$s->finish;
					}
					$self->{'dbh'}->disconnect;
					undef($self->{'dbh'});
					sleep(1);

					# We need to figure out if we're in a transaction and how far into
					# it we are.  If this is further than the first query in a
					# transaction, we can't retry.
					if($self->{'dbh'}{'AutoCommit'}==0) {
						$self->{'querycount'}++;
						if($self->{'querycount'}>1) {
							$maxattempts=1;
						}
					}

					if ($DBI::err) { $error = "$error - $DBI::err"; }
					die ($error);
				} else {
					# warn "Done!!";
					$done=1;
				}
			};
			if ($@) {
				# send that die back one more time
				die ($@);
			}

			# warn "Done = $done / Attempt = $attempt / Max Attempts = $maxattempts";

		}

		if($done==1) {
			return($s);
		} else {
			my $dieMsg = "Database Error: \n";
			$dieMsg .= "eval error $@ \n" if ($@);
			$dieMsg .= "ERROR: $error \n" if ($error);
			$dieMsg .= "DBI ERROR: $self->{'_err'} \n" if ($self->{'_err'});
			$dieMsg .= "QUERY: $query \n" if ($query);
			$dieMsg .= "ATTEMPS: $attempt|$maxattempts|$attemptPause \n";
			$dieMsg .= "STDERR: $self->{'stderr'} \n" if ($self->{'stderr'});
			die($dieMsg);
		}
	}

	sub notbaderr {
		my($self)=shift;
		my($error)=@_;
		my(%okerrs, @okerrs, $okerr, $ret);
		# 2528 is that weird shit that happens when you call sysmon
		#@okerrs=(2528,304201,405801,602801,3261);
		@okerrs=(2528);
		$ret=1;

		foreach $okerr (split(/,/, $self->{'notbaderr'})) {
			push (@okerrs, $okerr);
		}


		%okerrs=map { $_ => 1 } @okerrs;

		# is it bad or not?
		if(defined($okerrs{$error})) {
			$ret=1;
		} else {
			$ret=0;
		}

		return($ret);
	}

=pod

=item *
quote(I<$string>)

Quotes a string for a database query, will connect to the database if
necessary.  It is necessary to have an open database connection so that we
will know how we need to quote it.  Returns the quoted string.

=cut

	sub quote {
		my($self, $str)=@_;
		$self->openDB;
		return($self->{'dbh'}->quote($str));
	}

=pod

=item *
safestr(I<$string>)

Makes a string safe for use in a query.  Same as quote, but does not
actually enclose the string in quotes.

=cut

	sub safestr {
		my($self, $str)=@_;
		my($quoted);
		$self->openDB;
		$quoted=$self->{'dbh'}->quote($str);
		# take off the first and last characters...the quotes
		$quoted=~s/.(.*)/$1/g;
		chop($quoted);
		return($quoted);
	}

=pod

=item *
use(I<$dbname>)

Use the database I<$dbname>.

=cut

	sub use {
		my($self, $dbname)=@_;
		if(defined($dbname)) {
			$dbname=~s/\'//g;
			$self->doQuery("use $dbname");
		}
	}

=pod

=item *
autocommit(I<$flag>)

Change or describe the state of the autocommit flag.  If the argument
I<$flag> is given, it will set autocommit to that value (either 0 for
manual commits, or 1 for autocommits) and return the old value, else it
will just return the current value.

If you try to set it to anything other than 0 or 1, it will not be changed.

=cut

	sub autocommit {
		my($self, $flag)=@_;
		my($ret);

		$self->openDB;

		$ret=$self->{'dbh'}{'AutoCommit'};

		if(defined($flag) && ($flag==0 || $flag==1) ) {
			$self->{'dbh'}{'AutoCommit'}=$flag;
		}

		$self->{'ac'}=$flag;

		return($ret);
	}
}

1;

__END__
=pod

=back

=head1 AUTHOR

Dustin Sallings <dustin@beyond.com>

=head1 VERSION

$Id: DB.pm,v 1.2 2000/05/27 08:28:34 dustin Exp $
