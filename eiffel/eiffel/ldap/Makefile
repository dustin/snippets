# $Id: Makefile,v 1.4 1999/06/06 22:45:31 dustin Exp $
#
# Copyright (c) 1999 Dustin Sallings
# See forum.txt for license

E=smalleiffel
EC=$(E) compile

RPATH=-rpath /usr/local/lib

EFLAGS=-g -DOPENLDAP=1 -I/usr/local/include -debug_check
LFLAGS=-L/usr/local/lib $(RPATH) -llber -lldap

FILES=Makefile  ldaptest.e ldap.e openldap_stuff.c openldap_stuff.h

DEST=/home/dustin/public_html/eiffel/eiffeldap.tgz

# Begin targets

ldaptest: ldaptest.e ldap.e openldap_stuff.c
	$(EC) ldaptest -o $@ $(EFLAGS) openldap_stuff.c $(LFLAGS)

short:
	$(E) short ldap

dist: eiffeldap.tgz

eiffeldap.tgz: $(FILES)
	mkdir eiffeldap
	cp ../forum.txt eiffeldap
	cp $(FILES) eiffeldap
	tar cvf - eiffeldap | gzip -9vc > $@
	rm -rf eiffeldap
	cp eiffeldap.tgz $(DEST)
	chmod 644 $(DEST)

clean:
	$(E) clean ldaptest
	rm -f ldaptest *.o
