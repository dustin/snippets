# Copyright (c) 1998  Dustin Sallings
#
# $Id: DCache.pm,v 1.11 1998/12/16 23:23:32 dustin Exp $
#
# This is a CGI document caching system.

package DCache;

use strict;
use MD5;

sub new
{
    shift;
    my $self={};

    $self->{cachedir}="/tmp/dcache";

    bless($self);
    return($self);
}

sub cachedir
{
    my($self, $d)=@_;
    $self->{cachedir}=$d;
}

sub getname
{
    my($self, $in)=@_;
    my($md, $dig);

    $md=MD5->new;
    $md->add($in);
    $dig=$md->hexdigest;
    $dig=~s/(.{2})(.{16}).*/$1\/$2/g;
    $dig=$self->{'cachedir'}."/$dig";
    return($dig);
}

sub ensurepath
{
    my($fn)=@_;
    my(@a, $np);

    if(-d $fn) {
        return;
    }

    @a=split(/\//, $fn);

    pop(@a);
    $np=join('/', @a);
    &ensurepath($np);

    if(! -d $np ) {
        mkdir($np, 0755);
    }
}

sub cache
{
    my($self, $id, $mime, $data)=@_;
    my($name);

    $name=$self->getname($id);
    ensurepath($name);

	$id=$self->fixid($id);

    open(DC_OUT, ">$name");
    print DC_OUT "X-Cache: $id\n";
    print DC_OUT "Content-type: $mime\n\n";
    print DC_OUT $data;
    close(DC_OUT);
}

sub getcache
{
    my($self, $id, $data)=@_;
    my($name, $out, $buf);

    $name=$self->getname($id);
    open(DC_IN, $name) || return("");
    # Eat my tag
    <DC_IN>;
    $out="";
    if(defined($data)) {
	# Eat everything but the data
	$_=<DC_IN>;
	while(/\w/) {
	    $_=<DC_IN>;
        }
    }
    while( read(DC_IN, $buf, 1024) ) {
	$out.=$buf;
    }
    print <DC_IN>;
    close(DC_IN);
    return($out);
}

sub printcache
{
    my($self, $id)=@_;
    my($name, $buf);

    $name=$self->getname($id);
    open(DC_IN, $name) || return("");
    # Eat my tag
    <DC_IN>;
    while( read(DC_IN, $buf, 1024) ) {
        print $buf;
    }
    print <DC_IN>;
    close(DC_IN);
}

sub getcache_only
{
    my $self=shift;
    my $id=shift;
    my($name, $buf, $ret);

    $name=$self->getname($id);
    open(DC_IN, $name) || return("");
    # Eat everything but the data
    $_=<DC_IN>;
    while(/\w/) {
		$_=<DC_IN>;
    }
	$ret="";
    while( read(DC_IN, $buf, 1024) ) {
		$ret.=$buf;
    }
    print <DC_IN>;
    close(DC_IN);
	return($ret);
}

sub printcache_only
{
    my $self=shift;
    my $id=shift;
    my $fd=shift || \*STDOUT;
    my($name, $buf);

    $name=$self->getname($id);
    open(DC_IN, $name) || return("");
    # Eat everything but the data
    $_=<DC_IN>;
    while(/\w/) {
	$_=<DC_IN>;
    }
    while( read(DC_IN, $buf, 1024) ) {
        print $fd $buf;
    }
    print <DC_IN>;
    close(DC_IN);
}

sub fixid
{
	my($self, $id)=@_;

	$id=~s/[\s\n\r]/+/sog;
	$id;
}

sub checkcache
{
    my($self, $id, $compare)=@_;
    my($name, $r, @a, $s1, $s2);

    if(!defined($compare)) {
	 $compare="";
    }

    undef($r);
    $name=$self->getname($id);

	$id=$self->fixid($id);

    if(-f $name && (@a=stat(_))) {
	$s1=$a[9];
	if($compare ne "") {
	    if($compare=~/^\d+$/) {
		# Comparing lifetime
		if($s1>time()-$compare) {
		    $r=1;
		}
	    } else {
		# Must be a filename...
		@a=stat($compare);
	        $s2=$a[9];
	        if($s1>$s2) {
		    $r=1;
	        }
	    }
	} else {
	    open(DC_IN, $name);
	    $_=<DC_IN>;
	    if(/X-Cache: (.*)/) {
	        if($id eq $1) {
		    $r=1;
	        }
	    }
	    close(DC_IN);
	}
    }
    return($r);
}

1;

__END__

=head1 NAME

DCache.pm - Dustin's Cache Thing

=head1 SAMPLE USAGE

use DCache;

$c=DCache->new;

if($c->checkcache($somekey))
{
    print $c->getcache($somekey);
    exit(0);
}

$out=doabunchofstuff();

print $out;
$c->cache($somekey, "text/plain", $out);

=head1 METHODS

=item new;

    Create a new cache object.

=item cachedir($path)

    Set the caching directory to $path

=item checkcache($somekey)

or

=item checkcache($somekey, $compare)

    Check the cache for $somekey.  If no $compare is given, it just
checks for the existence of it, returns 1 if it exists, 0 if it
doesn't.  If $compare is there, it behaves one of two ways.  If
$compare is a number, it returns 1 if the cache exists and the data
is less than $compare seconds old.  If it's a path, it returns 1 if
the cache exists and the cache file was modified since a file
pointing to $compare (or $compare doesn't exist).

=item cache($somekey, $mimetype, $data)

    Cache $data keyed under $somekey using $mimetype as the Content-type

=item getcache($somekey)

    Return the data associated with $somekey

=item getname($somekey)

    Get the path to the cache file that data will be stored in for
$somekey

=cut
