#!/usr/local/bin/perl
#
# Perl implementation of RFC 2104, HMAC, using MD5

use MD5; #see, told you I was using MD5#
use strict;

sub hexprint
{
    map { printf "%2x", $_ } unpack("C16", $_[0]);
    print "\n";
}

sub hmac_md5
{
    my($text, $tlen, $key, $klen)=@_;
    my($md5, @i, @o);

    $md5=MD5->new;
    $md5->reset;

    @i=map{$_^0x36}unpack("C$klen", $key);
    while(@i<64) { push(@i, 0x36) }
    @o=map{$_^0x5c}unpack("C$klen", $key);
    while(@o<64) { push(@o, 0x5c) }

    # $md5->add( pack("c*", @i, unpack("C$tlen", $text)) );
    # $out=$md5->digest;
    # $md5->reset;
    # $md5->add( pack("c*", @o, unpack("C16", $out)) );
    # $out=$md5->digest;

    return($md5->hash( pack("c*", @o, unpack("C16",
		 $md5->hash(pack("c*",@i,unpack("C$tlen",$text)))))));

}

my $h=hmac_md5("what do ya want for nothing?", 28, "Jefe", 4);

hexprint($h);
