DEFINES=-DDEBUG
CFLAGS=-Wall -O6 -m486 -pipe $(DEFINES)
LIBS=-L/usr/X11R6/lib -lX11
LDFLAGS=-s
OBJS=X11.o main.o
SOURCES=X11.c main.c
OTHER=
EXES=
NAME=
JUNK=$(OBJS) $(EXES)

all:

tgz: $(SOURCES)
	@tar -czvf $(NAME).tgz Makefile $(SOURCES) $(OTHER)

clean:
	@echo Cleaning up...
	@rm -f $(JUNK)
