# Copyright (c) 1998  dustin sallings <dustin@spy.net>
# $Id: IOCache.pm,v 1.2 1998/12/02 09:21:46 dustin Exp $

=pod

=head1 NAME

IOCache - Object that allows transparent caching of all STDOUT

=head1 SYNOPSIS

  use IOCache;

  my $cache=IOCache->new('cache key', maxage => I<$AgeInSeconds>);

  [...]

=head1 DESCRIPTION

This module allows one to cache all output from a perl script completely
transparently.  You simply create an object, and the output of the script
is cached for the entire scope of the object.

The argument I<maxage> allows you to specify how long something is cached
in seconds.  If the cache isn't more than I<$AgeInSeconds> seconds old, the
cache will be displayed directly, and the script will immediately exit.  If
the cache does not exist, or is too old, the script will run, with all its
output saved to a temporary file, then the file's contents will be
displayed and the document will be cached.

=cut

use DCache;

{
	package IOCache;

	# The magic, part one
	sub new {
		my $proto = shift;
		my $class = ref($proto) || $proto;
		my($key, %options)=@_;
		my $self = {};

		if(!defined($key)) {
			die("Key must be defined to create IOCache object");
		}

		$self->{'key'}=$key;

		$self->{'dcache'}=DCache->new;
		if(defined($options{'cachedir'})) {
			$self->{'dcache'}->cachedir($options{'cachedir'});
		}

		if(-f "/tmp/$key") {
			$self->{'stat'}=[stat("/tmp/$key")];
		}

		# check for cache based on age if given, else, check for any cache,
		# if valid cache is found, print it out, and exit
		if(defined($options{'maxage'}) && defined($self->{'stat'})) {
			if($self->{'dcache'}->checkcache($key, $options{'maxage'})) {
				$self->{'dcache'}->printcache_only($key);
				print "!!! This was straigt-outta cache !!!\n";
				exit;
			}
		} elsif($self->{'dcache'}->checkcache($key)) {
			$self->{'dcache'}->printcache_only($key);
			print "!!! This was straigt-outta cache !!!\n";
			exit;
		}

		open(__IOCACHE_SAVESTDOUT, ">&STDOUT");
		$self->{'stdout'}=__IOCACHE_SAVESTDOUT;
		open(STDOUT, ">/tmp/iocache.$key.tmp.$$");

		bless($self);
		return($self);
	}

	# the magic, part two
	sub DESTROY {
		my $self=shift;
		my($stuff, $key);

		if(!defined($self->{'stdout'})) {
			return;
		}

		$key=$self->{'key'};

		close(STDOUT);
		open(STDOUT, ">&$self->{'stdout'}");

		# if we have any output, cache it.
		if(-s "/tmp/iocache.$key.tmp.$$") {
			# read it in
			open(__IOCACHE_READIN, "/tmp/iocache.$key.tmp.$$");
			# unlink here, just in case.
			unlink("/tmp/iocache.$key.tmp.$$");
			$stuff=join('', <__IOCACHE_READIN>);
			close(__IOCACHE_READIN);

			# and cache it.
			$self->{'dcache'}->cache($key, "IOCache", $stuff);

			# oh, and display it  :)
			print $stuff;
		}
	}
}

1;
