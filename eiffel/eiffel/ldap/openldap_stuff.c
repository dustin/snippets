/*
 * Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
 *
 * $Id: openldap_stuff.c,v 1.9 1999/06/07 19:50:35 dustin Exp $
 * See forum.txt for licensing information.
 */

#ifdef OPENLDAP

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include "openldap_stuff.h"

void
c_ldap_destroy(LDAP_HANDLE * h)
{
	if (h) {
		if (h->ldap_host)
			free(h->ldap_host);
		if (h->search_base)
			free(h->search_base);
		if (h->query_filter)
			free(h->query_filter);
		if (h->result_attribute)
			free(h->result_attribute);
		if (h->binddn)
			free(h->binddn);
		if (h->value_tmp)
			free(h->value_tmp);
		if (h->bindpw)
			free(h->bindpw);
		if (h->attr_values)
			ldap_value_free(h->attr_values);
		if (h->mods)
			ldap_mods_free(h->mods, 1);
		if (h->res)
			ldap_msgfree(h->res);
		if (h->ld)
			ldap_unbind(h->ld);
		free(h);
	}
}

LDAP_HANDLE *
c_ldap_init(char *host, int port)
{
	LDAP_HANDLE *ldap;

	ldap = calloc(1, sizeof(LDAP_HANDLE));
	assert(ldap);

	if (host)
		ldap->ldap_host = strdup(host);

	ldap->ldap_port = port;

	ldap->timeout = 5;

	ldap->ld = ldap_init(host, port);

	if (ldap->ld == NULL) {
		c_ldap_destroy(ldap);
		ldap = NULL;
	}
	return (ldap);
}

int
c_ldap_bind(LDAP_HANDLE * ldap, char *who, char *passwd)
{
	int     ret;

	assert(ldap);
	assert(ldap->ld);

	if (who)
		ldap->binddn = strdup(who);
	else
		ldap->binddn = NULL;

	if (passwd)
		ldap->bindpw = strdup(passwd);
	else
		ldap->bindpw = NULL;

	ret = ldap_simple_bind_s(ldap->ld, who, passwd);

	/* If it's 0, we're right */
	return (ret == 0);
}

int
c_ldap_search(LDAP_HANDLE * ldap, char *filter, int scope)
{
	int     rc;
	LDAPMessage *res;
	struct timeval tv;

	assert(ldap);
	assert(ldap->ld);
	assert(filter);

	tv.tv_sec = ldap->timeout;
	tv.tv_usec = 0;

	if (ldap->query_filter)
		free(ldap->query_filter);

	ldap->query_filter = strdup(filter);

	rc = ldap_search_st(ldap->ld, ldap->search_base, scope, filter, 0, 0, &tv,
	    &(ldap->res));

	ldap->last_error=ldap_result2error(ldap->ld, ldap->res, 0);

	return (rc == LDAP_SUCCESS);
}

int
c_ldap_set_sb(LDAP_HANDLE * ldap, char *sb)
{
	assert(ldap);

	if (ldap->search_base)
		free(ldap->search_base);

	if (sb)
		ldap->search_base = strdup(sb);
	else
		ldap->search_base = NULL;
}

int
c_ldap_set_timeout(LDAP_HANDLE * ldap, int timeout)
{
	assert(ldap);

	ldap->timeout = timeout;
}

int
c_ldap_nresults(LDAP_HANDLE * ldap)
{
	int     ret = 0;

	assert(ldap);
	assert(ldap->ld);
	assert(ldap->res);

	if (ldap->ld && ldap->res)
		ret = ldap_count_entries(ldap->ld, ldap->res);

	return (ret);
}

int
c_ldap_first_entry(LDAP_HANDLE * ldap)
{
	assert(ldap);
	assert(ldap->ld);
	assert(ldap->res);

	ldap->entry = ldap_first_entry(ldap->ld, ldap->res);
	return (ldap->entry != NULL);
}

int
c_ldap_next_entry(LDAP_HANDLE * ldap)
{
	assert(ldap);
	assert(ldap->ld);
	assert(ldap->entry);

	ldap->entry = ldap_next_entry(ldap->ld, ldap->entry);
	return (ldap->entry != NULL);
}

char   *
c_ldap_first_attribute(LDAP_HANDLE * ldap)
{
	char   *ret;

	assert(ldap);
	assert(ldap->ld);
	assert(ldap->entry);

	ret = ldap_first_attribute(ldap->ld, ldap->entry, &(ldap->ber));
	return (ret);
}

char   *
c_ldap_next_attribute(LDAP_HANDLE * ldap)
{
	char   *ret;

	assert(ldap);
	assert(ldap->ld);
	assert(ldap->entry);
	assert(ldap->ber);

	ret = ldap_next_attribute(ldap->ld, ldap->entry, ldap->ber);
	return (ret);
}

char   *
c_ldap_get_value(LDAP_HANDLE * ldap, char *attribute, int index)
{
	char  **values;
	int     nvalues;

	assert(ldap);
	assert(ldap->ld);
	assert(ldap->entry);

	values = ldap_get_values(ldap->ld, ldap->entry, attribute);

	if (ldap->value_tmp) {
		free(ldap->value_tmp);
		ldap->value_tmp = NULL;
	}
	if (values) {
		for (nvalues = 0; values[nvalues]; nvalues++);

		if (index < nvalues)
			ldap->value_tmp = strdup(values[index]);

		ldap_value_free(values);
	}
	return (ldap->value_tmp);
}

int
c_ldap_compare(LDAP_HANDLE * ldap, char *dn, char *attr, char *value)
{
	int     rc;
	assert(ldap);
	assert(ldap->ld);

	rc = ldap_compare_s(ldap->ld, dn, attr, value);

	return (rc == LDAP_COMPARE_TRUE);
}

static void
_c_ldap_add_something(LDAP_HANDLE * ldap, char *attr, char *value,
    int vlen, int op)
{
	int     i = 0, j = 0;
	struct berval *bvp;

	assert(ldap);
	assert(attr);

	op |= LDAP_MOD_BVALUES;

	/* Find the starting point */
	if (ldap->mods != NULL) {
		for (i = 0; ldap->mods[i] != NULL; i++) {
			if (strcasecmp(ldap->mods[i]->mod_type, attr) == 0) {
				break;
			}
		}
	}
	/* Grow it if we need to. */
	if (ldap->mods == NULL || ldap->mods[i] == NULL) {
		ldap->mods = (LDAPMod **) realloc(ldap->mods,
		    (i + 2) * sizeof(LDAPMod *));
		assert(ldap->mods);
	}
	/* Allocate memory for it */
	ldap->mods[i + 1] = NULL;
	ldap->mods[i] = (LDAPMod *) calloc(1, sizeof(LDAPMod));
	assert(ldap->mods[i]);

	/* Set type */
	ldap->mods[i]->mod_op = op;

	/* Add attribute type */
	ldap->mods[i]->mod_type = strdup(attr);
	assert(ldap->mods[i]->mod_type);

	/* Add the value */

	if (value) {
		/* Go to the end (if there's a beginning) */
		if (ldap->mods[i]->mod_bvalues != NULL) {
			for (j = 0; ldap->mods[i]->mod_bvalues[j] != NULL; ++j);
		}
		/* Grow it */
		ldap->mods[i]->mod_bvalues = (struct berval **) realloc(
		    ldap->mods[i]->mod_bvalues, (j + 2) * sizeof(struct berval *));
		assert(ldap->mods[i]->mod_bvalues);

		/* Get the value and add it */
		ldap->mods[i]->mod_bvalues[j + 1] = NULL;
		bvp = (struct berval *) calloc(1, sizeof(struct berval));
		assert(bvp);
		ldap->mods[i]->mod_bvalues[j] = bvp;

		/* Allocate for value */
		bvp->bv_len = vlen;
		bvp->bv_val = (char *) calloc(1, vlen + 1);
		assert(bvp->bv_val);

		memcpy(bvp->bv_val, value, vlen);
		bvp->bv_val[vlen] = '\0';
	}
}

void
c_ldap_mod_add(LDAP_HANDLE * ldap, char *attr, char *value, int vlen)
{
	_c_ldap_add_something(ldap, attr, value, vlen, LDAP_MOD_ADD);
}

void
c_ldap_mod_replace(LDAP_HANDLE * ldap, char *attr, char *value, int vlen)
{
	_c_ldap_add_something(ldap, attr, value, vlen, LDAP_MOD_REPLACE);
}

void
c_ldap_mod_delete(LDAP_HANDLE * ldap, char *attr, char *value, int vlen)
{
	_c_ldap_add_something(ldap, attr, 0, 0, LDAP_MOD_DELETE);
}

int
c_ldap_add(LDAP_HANDLE * ldap, char *dn)
{
	int     rc;

	assert(ldap);
	assert(ldap->ld);
	assert(ldap->mods);

	rc = ldap_add_s(ldap->ld, dn, ldap->mods);

	return (rc == LDAP_SUCCESS);
}

int
c_ldap_modify(LDAP_HANDLE * ldap, char *dn)
{
	int     rc;

	assert(ldap);
	assert(ldap->ld);
	assert(ldap->mods);

	rc = ldap_modify_s(ldap->ld, dn, ldap->mods);

	return (rc == LDAP_SUCCESS);
}

int
c_ldap_delete(LDAP_HANDLE * ldap, char *dn)
{
	int     rc;

	assert(ldap);
	assert(ldap->ld);

	rc = ldap_delete_s(ldap->ld, dn);
	return (rc == LDAP_SUCCESS);
}

void
c_ldap_mod_clean(LDAP_HANDLE * ldap)
{
	void   *p;

	assert(ldap);

	/* There's a bug in here somewhere, this is so that if this thing
	 * causes a segmentation violation, we won't get it again */
	p = ldap->mods;
	ldap->mods = NULL;

	if (p)
		ldap_mods_free(p, 1);
}

/* Get a message describing the last error */
char *
c_ldap_err_msg(LDAP_HANDLE *ldap)
{
	assert(ldap);
	assert(ldap->ld);

	return(ldap_err2string(ldap->last_error));
}

#endif
