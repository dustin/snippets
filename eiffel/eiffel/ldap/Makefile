# $Id: Makefile,v 1.5 1999/06/07 07:35:01 dustin Exp $
#
# Copyright (c) 1999 Dustin Sallings
# See forum.txt for license

E=smalleiffel
EC=$(E) compile

VERSION="1.0"
FN="eiffeldap-$(VERSION)"

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
	mkdir $(FN)
	cp ../forum.txt $(FN)
	cp $(FILES) $(FN)
	tar cvf - $(FN) | gzip -9vc > $@
	rm -rf $(FN)
	cp eiffeldap.tgz $(DEST)
	chmod 644 $(DEST)

clean:
	$(E) clean ldaptest
	rm -f ldaptest *.o
