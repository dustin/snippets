#ifdef OPENLDAP

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include "openldap_stuff.h"

void
c_ldap_destroy(LDAP_HANDLE *h)
{
	if(h) {
		if(h->ldap_host)
			free(h->ldap_host);
		if(h->search_base)
			free(h->search_base);
		if(h->query_filter)
			free(h->query_filter);
		if(h->result_attribute)
			free(h->result_attribute);
		if(h->binddn)
			free(h->binddn);
		if(h->bindpw)
			free(h->bindpw);
		if(h->attr_values)
			ldap_value_free(h->attr_values);
		if(h->res)
			ldap_msgfree(h->res);
		if(h->ld)
			ldap_unbind(h->ld);
		free(h);
	}
}

LDAP_HANDLE *
c_ldap_init(char *host, int port)
{
	LDAP_HANDLE *ldap;

	ldap=calloc(1, sizeof(LDAP_HANDLE));
	assert(ldap);

	if(host)
		ldap->ldap_host=strdup(host);

	ldap->ldap_port=port;

	ldap->timeout=5;

	ldap->ld=ldap_init(host, port);

	if(ldap->ld == NULL) {
		c_ldap_destroy(ldap);
		ldap=NULL;
	}

	return(ldap);
}

int
c_ldap_bind(LDAP_HANDLE *ldap, char *who, char *passwd)
{
	int ret;

	assert(ldap);

	if(who)
		ldap->binddn=strdup(who);

	if(passwd)
		ldap->bindpw=strdup(passwd);

	ret=ldap_simple_bind_s(ldap->ld, who, passwd);

	/* If it's 0, we're right */
	return(ret==0);
}

int
c_ldap_search(LDAP_HANDLE *ldap, char *filter, int scope)
{
	int rc;
	LDAPMessage *res;
	struct timeval tv;

	tv.tv_sec=ldap->timeout;
	tv.tv_usec=0;

	assert(ldap);
	assert(filter);

	ldap->query_filter=strdup(filter);

	rc = ldap_search_st(ldap->ld, ldap->search_base, scope, filter, 0, 0, &tv,
		&(ldap->res));

	return(rc==LDAP_SUCCESS);
}

int
c_ldap_first_entry(LDAP_HANDLE *ldap)
{
	ldap->entry=ldap_first_entry(ldap->ld, ldap->res);
	return(ldap->entry != NULL);
}

int
c_ldap_set_sb(LDAP_HANDLE *ldap, char *sb)
{
	if(ldap->search_base)
		free(ldap->search_base);

	ldap->search_base=strdup(sb);
}

int
c_ldap_set_timeout(LDAP_HANDLE *ldap, int timeout)
{
	ldap->timeout=timeout;
}

int
c_ldap_nresults(LDAP_HANDLE *ldap)
{
	int ret=0;
	if(ldap->ld && ldap->res)
		ret=ldap_count_entries(ldap->ld, ldap->res);

	return(ret);
}

#endif
