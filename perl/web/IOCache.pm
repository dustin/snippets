# Copyright (c) 1998  dustin sallings <dustin@spy.net>
# $Id: IOCache.pm,v 1.10 2000/04/25 23:50:23 dustin Exp $

=pod

=head1 NAME

IOCache - Object that allows transparent caching of all STDOUT

=head1 SYNOPSIS

  use IOCache;
  
  my $cache=IOCache->new('cache key', [tagged options]);
   
  [...]

=head1 DESCRIPTION

This module allows one to cache all output from a perl script completely
transparently.  You simply create an object, and the output of the script
is cached for the entire scope of the object.

The option I<maxage> allows you to specify how long something is cached
in seconds.  If the cache isn't more than I<$AgeInSeconds> seconds old, the
cache will be displayed directly, and the script will immediately exit.  If
the cache does not exist, or is too old, the script will run, with all its
output saved to a temporary file, then the file's contents will be
displayed and the document will be cached.  If there is no I<maxage>
option, then any matching cache is valid.

The option I<cachedir> allows you to specify an alternate C<DCache> cache
directory.

The option I<cachetag> will cause a cache hit to include the given cache
tag at the bottom of the output, letting you know that it got it from
cache.  For instance, you may want to do something like this for debugging:

 $cache=IOCache->new($key,
   cachetag => "<!-- This came from cache: $key -->\n");

=head1 EXAMPLE

  #!/usr/local/bin/perl
  
  use IOCache;
  
  my $cache=IOCache->new("test1", maxage => 5, cachedir => '/tmp/cache2',
                                  cachetag => "!!! WOO !!!\n");
  
  sleep 5;
  print "This will be cached...\n";

=head1 AUTHOR

Dustin Sallings <dustin@spy.net>

=head1 VERSION

$Id: IOCache.pm,v 1.10 2000/04/25 23:50:23 dustin Exp $

=cut

use DCache;
use Fcntl();
use MD5;

{
	package IOCache;

	# The magic, part one
	sub new {
		my $proto = shift;
		my $class = ref($proto) || $proto;
		my($key, %options)=@_;
		my $self = {};
		my($fkey);

		if(!defined($key)) {
			die("Key must be defined to create IOCache object");
		}

		bless($self);

		$self->{'key'}=$key;
		$fkey=$self->getfkey($key);
		$self->{'fkey'}=$fkey;
		$self->{'docache'}=1;

		$self->{'dcache'}=DCache->new;
		if(defined($options{'cachedir'})) {
			$self->{'dcache'}->cachedir($options{'cachedir'});
		}

		# check for cache based on age if given, else, check for any cache,
		# if valid cache is found, print it out, and exit
		if(defined($options{'maxage'})) {
			if($self->{'dcache'}->checkcache($key, $options{'maxage'})) {
				$self->{'dcache'}->printcache_only($key);
				if(defined($options{'cachetag'})) {
					print $options{'cachetag'};
				}
				exit;
			}
		} elsif($self->{'dcache'}->checkcache($key)) {
			$self->{'dcache'}->printcache_only($key);
			if(defined($options{'cachetag'})) {
				print $options{'cachetag'};
			}
			exit;
		}
		
		open(__IOCACHE_SAVESTDOUT, ">&STDOUT");
		$self->{'stdout'}=__IOCACHE_SAVESTDOUT;
		sysopen(STDOUT,
			"/tmp/iocache.$fkey.tmp.$$",
			Fcntl::O_CREAT()|Fcntl::O_RDWR()|Fcntl::O_EXCL(),
			0600);

		return($self);
	}

	sub nocache {
		my($self)=shift;
		self->{'docache'}=0;
	}

	# need a key we can use on the filesystem temporarily
	sub getfkey {
		my($self)=shift;
		my($key)=@_;
		my($md, $dig);

		$md=MD5->new;
		$md->add($key);
		$md->add($$);
		$md->add($<);
		$md->add($();
		$md->add($^T);
		$md->add(time());
		$md->add(rand());
		$dig=$md->hexdigest;
		return($dig);
	}

	# the magic, part two
	sub DESTROY {
		my $self=shift;
		my($stuff, $key, $fkey);

		if(!defined($self->{'stdout'})) {
			return;
		}

		$key=$self->{'key'};
		$fkey=$self->{'fkey'};

		close(STDOUT);
		open(STDOUT, ">&$self->{'stdout'}");

		# if we have any output, cache it.
		if(-s "/tmp/iocache.$fkey.tmp.$$") {
			# read it in
			open(__IOCACHE_READIN, "/tmp/iocache.$fkey.tmp.$$");
			# unlink here, just in case.
			unlink("/tmp/iocache.$fkey.tmp.$$");
			$stuff=join('', <__IOCACHE_READIN>);
			close(__IOCACHE_READIN);

			# This allows us to turn off the caching if an object decides
			# it shouldn't recache after it's been instantiated.
			if($self->{'docache'} == 1) {
				# and cache it.
				$self->{'dcache'}->cache($key, "IOCache", $stuff);
			}

			# oh, and display it  :)
			print $stuff;
		}
	}
}

1;
