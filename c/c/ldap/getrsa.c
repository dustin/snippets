/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: getrsa.c,v 1.1 2000/03/04 23:47:45 dustin Exp $
 */

/* This code should only be compiled if there is LDAP support */
#ifdef HAVE_LDAP

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <assert.h>

#include <lber.h>
#include <ldap.h>

#include <fcntl.h>

static LDAP    *
ldap_getld(void)
{
	LDAP           *ld;
	int             ret;
	char           *binddn, *server, *bindpw;

	binddn=NULL;
	bindpw=NULL;
	server="foo";

	ld = ldap_open(server, 0);
	if (ld == NULL) {
		return (NULL);
	}

	ret = ldap_bind_s(ld, binddn, bindpw, LDAP_AUTH_SIMPLE);
	if (ret != LDAP_SUCCESS) {
		return (NULL);
	}
	return (ld);
}

void
ldap_getuser(char *name)
{
	LDAP           *ld;
	LDAPMessage    *res = 0;
	char            filter[8192];
	char          **values;
	char           *base;
	int				times[2];
	char           *att[] = {
		"sshkey",
		0
	};

	sprintf(filter, "uid=%s", name);

	base="dc=spy,dc=net";

	ld = ldap_getld();
	if(ld==NULL)
		return;

	if (ldap_search_st(ld, base, LDAP_SCOPE_SUBTREE, filter, att, 0, 0, &res)
	    == -1) {
		ldap_unbind(ld);
		return;
	}

	values=ldap_get_values(ld, res, "sshkey");
	if(values && values[0]) {
		int i;
		printf("Key:\n");
		for(i=0; values[i]; i++) {
			puts(values[i]);
		}
	} else {
		printf("I've got no values.\n");
	}

	ldap_unbind(ld);
	return;
}

int main(int argc, char **argv) {
	assert(argc>1);

	ldap_getuser(argv[1]);
}

#endif				/* HAVE_LDAP */
