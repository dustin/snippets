# Generic database object.
#
# $Id: DB.pm,v 1.1 1999/01/04 02:51:49 dustin Exp $

=pod

=head1 NAME

SPY::DB - Abstracted DBI stuff, we like things to be *very* abstracted.  :)

=head1 SYNOPSIS

  use SPY::DB;

  $dbh=SPY::DB->new;

  $s=$dbh->doQuery($query);

  [...]

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
use strict;

{
	package SPY::DB;

	use Data::Dumper;

	sub new {
		my $proto=shift;
		my $class = ref($proto) || $proto;
		my $dbname = shift;
		my $dbhost = shift;

		my $self={};

		# Take a dbname as an arg to new if there is an arg to new
		if(defined($dbname)) {
			$self->{'_dbname'}=$dbname;
		}
		if(defined($dbhost)) {
			$self->{'_dbhost'}=$dbhost;
		}

		$self->{'querycount'}=0;
		bless($self);
		return($self);
	}

	sub DESTROY {
		my($self)=@_;

		if(defined($self->{'dbh'})) {
			$self->{'dbh'}->disconnect();
		}
	}

=pod

=item commit

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

=item rollback

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

=item hashQuery(I<$query>, I<%options>)

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
				eval $str;
				return(%{$VAR1});
			}
		}

		$s=$self->doQuery($query);
		while($r=$s->fetchrow_hashref) {
			my(%tmp, $k, $v);
			%tmp=%{$r};
			while( ($k,$v)=each(%tmp)) {
				push(@{$ret{$k}}, $v);
			}
		}
		if(defined($options{'cachetime'})) {
			$c->cache($query, 'hashQuery', Dumper(\%ret));
		}
		return(%ret);
	}

=pod

=item doQuery(I<$query>, I<%options>)

Perform a SQL query, will connect to a database if necessary, returns a DBI
query result handler thing.

B<WARNING>!:  This routine dies on failure, if you do not want this, call
it inside a block eval.

=cut

	sub doQuery {
		my $self=shift;
		my($query, %options)=@_;
		my($s,$dbh, $done, $attempt, $error, $maxattempts);

		# default
		$maxattempts=3;

		# We need to figure out if we're in a transaction and how far into
		# it we are.  If this is further than the first query in a
		# transaction, we can't retry.

		if($self->{'AutoCommit'}==0) {
			$self->{'querycount'}++;

			if($self->{'querycount'}>1) {
				$maxattempts=1;
			}
		}

		$done=$attempt=0;

		print "<!-- Doing query: $query\nCount is $self->{'querycount'}\n".
			"Retries is $maxattempts\n-->\n";

		while($done==0 && $attempt<$maxattempts) {
			$attempt++;
			eval {
				$self->openDB unless($self->{'dbh'});
				$s=$self->{'dbh'}->prepare($query);
				if(!$s) {
					$error=$DBI::errstr;
					$self->{'dbh'}->disconnect;
					undef($self->{'dbh'});
					sleep(1);
					die;
				}
				$s->execute;
				if(!$s || $DBI::err) {
					$error=$DBI::errstr;
					if($s) {
						$s->finish;
					}
					$self->{'dbh'}->disconnect;
					undef($self->{'dbh'});
					sleep(1);
					die;
				}
				$done=1;
			};
		}

		if($done==1) {
			return($s);
		} else {
			die("Database Error:  $error\n$query\n");
		}
	}

=pod

=item quote(I<$string>)

Quotes a string for a database query, will connect to the database if
necessary.  It is necessary to have an open database connection so that we
will know how we need to quote it.  Returns the quoted string.

=cut

	sub quote {
		my($self, $str)=@_;
		$self->openDB unless($self->{'dbh'});
		return($self->{'dbh'}->quote($str));
	}

	sub openDB {
		my($self)=shift;
		my($dbname, $dbhost, $dbuser, $dbpass);

		$dbname='misc';
		$dbuser='nobody';
		$dbhost='bleu';
		$dbpass='';

		if(defined($self->{'_dbname'})) {
			$dbname=$self->{'_dbname'};
		}

		if(defined($self->{'_dbhost'})) {
			$dbname=$self->{'_dbhost'};
		}

		if(defined($ENV{'REMOTE_USER'})) {
			$dbuser=$ENV{'REMOTE_USER'};
			$dbpass="";
		}

		$self->{'dbh'}=DBI->connect("dbi:Pg:dbname=$dbname;host=$dbhost",
			$dbuser,$dbpass) || die("Cannot connect to database\n");
		$self->{'dbh'}->{'AutoCommit'}=0;
	}

}

1;

__END__
=pod

=back

=head1 AUTHOR

Dustin Sallings <dustin@beyond.com>

=head1 VERSION

$Id: DB.pm,v 1.1 1999/01/04 02:51:49 dustin Exp $

=cut
