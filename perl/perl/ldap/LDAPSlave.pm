package LDAPSlave;
# Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
# $Id: LDAPSlave.pm,v 1.2 2000/02/10 21:21:35 dustin Exp $

use strict;
use Net::LDAP;

sub new {
	my($proto)=shift;
	my($class)=ref($proto) || $proto;
	my($name, $ldap)=@_;
	my $self={};
	$self->{'_name'}=$name;
	$self->{'_ldap'}=$ldap;
	bless($self, $class);

	return($self);
}

sub name {
	my($self)=shift;
	return($self->{'_name'});
}

sub ldap {
	my($self)=shift;
	return($self->{'_ldap'});
}

1;
