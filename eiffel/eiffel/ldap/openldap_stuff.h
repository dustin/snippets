#ifdef OPENLDAP

#include <lber.h>
#include <ldap.h>

typedef struct {
	char   *ldap_host;
	int     ldap_port;
	char   *search_base;
	char   *query_filter;
	char   *result_attribute;
	int     bind;
	char   *binddn;
	char   *bindpw;
	int     timeout;
	char  **attr_values;
	LDAP   *ld;
	LDAPMessage *res;
	LDAPMessage *entry;
	BerElement *ber;
} LDAP_HANDLE;

#endif /* OPENLDAP */
