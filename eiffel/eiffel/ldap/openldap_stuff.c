#ifdef OPENLDAP

#include <lber.h>
#include <ldap.h>

LDAP *
c_ldap_init(char *host, int port)
{
	return(ldap_init(host, port));
}

int
c_ldap_bind(LDAP *ld, char *who, char *passwd)
{
	int ret;

	ret=ldap_simple_bind_s(ld, who, passwd);

	/* If it's 0, we're right */
	return(ret==0);
}

#endif
