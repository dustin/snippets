# Copyright (c) 1998  Dustin Sallings
#
# $Id: DCache.pm,v 1.2 1998/01/15 06:47:19 dustin Exp $
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
    open(DC_IN, $name);
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
            @a=stat($compare);
	    $s2=$a[9];

	    if($s1>$s2)
	    {
		$r=1;
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
