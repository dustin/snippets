# $Id: Makefile,v 1.1 1997/08/26 06:00:12 dustin Exp $
DEFINES= #-DDEBUG
CFLAGS=-O2 $(DEFINES)
LIBS=-L. -lcgi
LDFLAGS=-s $(LIBS)
RANLIB=true
OBJS=libcgi.o parsecgi.o
SOURCES=libcgi.c parsecgi.c
OTHER=cgi.h testcgi.c
EXES=testcgi
NAME=libcgi
JUNK=$(OBJS) $(EXES) core libcgi.a

all: libcgi.a testcgi

libcgi.a: $(OBJS)
	ar rc libcgi.a $(OBJS)
	$(RANLIB) libcgi.a

testcgi: libcgi.a
	cc -o testcgi testcgi.c $(LDFLAGS)

tgz:
	@tar -cvf $(NAME).tar Makefile $(SOURCES) $(OTHER)
	@gzip -9v $(NAME).tar

clean:
	@echo Cleaning up...
	@rm -f $(JUNK)
