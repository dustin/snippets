SHELL=/bin/sh
VERSION_MAJOR=1
VERSION_MINOR=0
VERSION_PATCH=0
VERSION=$(VERSION_MAJOR).$(VERSION_MINOR).$(VERSION_PATCH)
DEFINES=-DVERSION=\"$(VERSION)\" -DMAX_X=80 -DMAX_Y=24
CC=CC
CFLAGS=-O2 +w $(DEFINES)
LIBS=
LDFLAGS=-s $(LIBS)
DOOR_OBJS=door.o
OBJS=$(DOOR_OBJS)
SOURCES=door.c
OTHER=door.h
EXES=doors
NAME=doors
NAME2=$(NAME)-$(VERSION)
JUNK=$(OBJS) $(EXES) core test *~

all:	doors

doors:	$(DOOR_OBJS)
	$(CC) $(CFLAGS) -o doors $(DOOR_OBJS) $(LDFLAGS)

.c.o:
	$(CC) $(CFLAGS) -c $*.c

dist:	$(SOURCES) $(OTHER)
	@mkdir $(NAME2)
	@cp Makefile $(SOURCES) $(OTHER) $(NAME2)
	@tar -cvf $(NAME2).tar $(NAME2)
	@gzip -9 $(NAME2).tar
	@rm -rf $(NAME2)

clean:
	@echo Cleaning up...
	@rm -f $(JUNK)
