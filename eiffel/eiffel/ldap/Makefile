E=smalleiffel
EC=$(E) compile

RPATH=-rpath /usr/local/lib

EFLAGS=-g -DOPENLDAP=1 -I/usr/local/include -debug_check
LFLAGS=-L/usr/local/lib $(RPATH) -llber -lldap

FILES=Makefile

ldaptest: ldaptest.e ldap.e openldap_stuff.c
	$(EC) ldaptest -o $@ $(EFLAGS) openldap_stuff.c $(LFLAGS)

short:
	$(E) short ldap

clean:
	$(E) clean ldaptest
	rm -f ldaptest *.o
