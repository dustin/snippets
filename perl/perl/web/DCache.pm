# Copyright (c) 1998  Dustin Sallings
#
# $Id: DCache.pm,v 1.3 1998/01/15 07:29:33 dustin Exp $
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
    $dig=~s/(.{2})(.{16}).*/\1\/\2/g;
    $dig=$self->{cachedir}."/$dig";
    return($dig);
}

sub ensurepath
{
    my($fn)=@_;
    my(@a, $np);

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

sub cache
{
    my($self, $id, $mime, $data)=@_;
    my($name);

    $name=getname($self, $id);
    ensurepath($name);

    open(DC_OUT, ">$name");
    print DC_OUT "X-Cache: $id\n";
    print DC_OUT "Content-type: $mime\n\n";
    print DC_OUT $data;
    close(DC_OUT);
}

sub getcache
{
    my($self, $id)=@_;
    my($name, $out);

    $name=getname($self, $id);
    open(DC_IN, $name) || return("");
    # Eat my tag
    <DC_IN>;
    $out="";
    while(<DC_IN>)
    {
	$out.=$_;
    }
    close(DC_IN);
    return($out);
}

sub checkcache
{
    my($self, $id, $compare)=@_;
    my($name, $r, @a, $s1, $s2);

    $r=0;
    $name=getname($self, $id);
    if(-f $name && (@a=stat(_)))
    {
	$s1=$a[9];
	if($compare ne "")
	{
	    if($compare=~/^\d+$/)
	    {
		# Comparing lifetime
		if($s1>time()-$compare)
		{
		    $r=1;
		}
	    }
	    else
	    {
		# Must be a filename...
		@=stat($compare);
	        $s2=$a[9];
	        if($s1>$s2)
	        {
		    $r=1;
	        }
	    }
	}
	else
	{
	    open(DC_IN, $name);
	    $_=<DC_IN>;
	    if(/X-Cache: (.*)/)
	    {
	        if($id eq $1)
	        {
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
