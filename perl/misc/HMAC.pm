#!/usr/local/bin/perl
# Copyright (c) 1998 Dustin Sallings
#
# $Id: HMAC.pm,v 1.3 1998/05/06 16:30:37 dustin Exp $

package HMAC;

use MD5;
use strict;

sub new
{
    my $proto=shift;
    my $type=(shift||"MD5");
    my $blocksize=(shift||64);
    my $self={};

    $self->{'type'}=$type;
    $self->{'blocksize'}=$blocksize;
    bless($self);
    return($self);
}

sub hexprint
{
    my($self)=shift;
    my($out);
    $out=join('',map{sprintf "%2x",$_}unpack("C16",$_[0]));
    $out=~s/\s/0/g;
    return($out);
}

sub do_md5
{
    my($self, $text, $key)=@_;
    my($md5, @i, @o);

    $md5=MD5->new;

    @i=map{$_^0x36}unpack("C*", $key);
    while(@i<$self->{'blocksize'}) { push(@i, 0x36) }
    @o=map{$_^0x5c}unpack("C*", $key);
    while(@o<$self->{'blocksize'}) { push(@o, 0x5c) }

    return($md5->hash( pack("c*", @o, unpack("C16",
		 $md5->hash(pack("c*",@i,unpack("C*",$text)))))));

}

sub do
{
    my($self)=@_;
    my(%types);

    %types=("MD5" => \&do_md5);

    if(defined($types{$self->{'type'}})) {
	&{$types{$self->{'type'}}}(@_);
    } else {
	print "Cannot do type $self->{'type'} yet.\n";
    }
}

sub test
{
    my($self)=@_;
    my($h);
    print "Should print:\n0x750c783e6ab0b503eaa86e310a5db738\n";
    $h=$self->do("what do ya want for nothing?", "Jefe");
    $h=$self->hexprint($h);
    print "0x$h\n";
}

1;

__END__

=head1 NAME

HMAC.pm - Keyed-Hashing for Message Authentication

=head1 SYNOPSIS

   use HMAC;

   $hmac=HMAC->new($hashtype, $blocksize);

   $res=$hmac->do($text, $key);

=head1 DESCRIPTION

The B<HMAC> module provides a perl interface to the HMAC Keyed-Hashing for
Message Authentication as described in RFC 2104.

=head1 USAGE

C<$hmac=HMAC-E<gt>new($hashtype, $blocksize);> will create a new hmac object
using C<$hashtype> for the hashing algorithm and C<$blocksize> for the key
block size.  Currently, only the MD5 hashing algorithm is supported.  Both the
hashtype and the block size are optional, and the defaults of MD5 for the
hash and 64 bytes for the block size will be used if no arguments are
given.

Once you create your object, you can get your results via the C<do> method,
i.e.:  C<$res=$hmac-E<gt>do($text, $key);>.

=head1 AUTHOR

This module was written by Dustin Sallings E<lt>dustin@spy.netE<gt>
