package LDAPSlave;

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
