# $Id: Makefile,v 1.1 1997/08/26 06:00:47 dustin Exp $
DEFINES= #-DDEBUG
INCLUDES=-I/usr/people/dustin/include -I/usr/people/dustin/lib/msql/include
CFLAGS=-O2 $(INCLUDES) $(DEFINES)
LIBS=-L/usr/people/dustin/lib -L/usr/people/dustin/lib/mql/lib -lcgi -lmsql
LDFLAGS=-s $(LIBS)
OBJS=
SOURCES=
OTHER=
EXES=
NAME=
JUNK=$(OBJS) $(EXES) core

tgz:
	@tar -cvf $(NAME).tar Makefile $(SOURCES) $(OTHER)
	@gzip -9v $(NAME).tar

clean:
	@echo Cleaning up...
	@rm -f $(JUNK)
