#!/usr/local/bin/perl
# Copyright (c) 1998 Dustin Sallings
#
# $Id: HMAC.pm,v 1.2 1998/05/06 08:23:13 dustin Exp $

package HMAC;

use MD5;
use strict;

sub new
{
    my $proto=shift;
    my $type=(shift||"MD5");
    my $self={};

    $self->{type}=$type;
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
    my($self, $text, $tlen, $key, $klen)=@_;
    my($md5, @i, @o);

    $md5=MD5->new;

    @i=map{$_^0x36}unpack("C$klen", $key); while(@i<64) { push(@i, 0x36) }
    @o=map{$_^0x5c}unpack("C$klen", $key); while(@o<64) { push(@o, 0x5c) }

    return($md5->hash( pack("c*", @o, unpack("C16",
		 $md5->hash(pack("c*",@i,unpack("C$tlen",$text)))))));

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
    $h=$self->do("what do ya want for nothing?", 28, "Jefe", 4);
    $h=$self->hexprint($h);
    print "0x$h\n";
}

1;

__END__

=head1 NAME

HMAC.pm - HMAC library following RFC 2104.

=head1 USAGE

use HMAC;

$hmac=HMAC->new('MD5');  # Returns a new HMAC_MD5 object;

$res=$hmac->do($text, $tlen, $key, $klen); # do calculation

=end
