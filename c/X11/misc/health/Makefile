DEFINES= #-DDEBUG -DALARMLENGTHDEFAULT=5
CFLAGS=-Wall -O6 -m486 -pipe -fomit-frame-pointer $(DEFINES)
LIBS=-L/usr/X11R6/lib -lX11
LDFLAGS=-s $(LIBS)
OBJS=X11.o main.o
SOURCES=X11.c main.c
OTHER=xhealth.h
EXES=xhealth
NAME=xhealth
JUNK=$(OBJS) $(EXES) *~

all: xhealth

xhealth: $(OBJS)
	cc -$(CFLAGS) -o $(NAME) $(OBJS) $(LDFLAGS)

tgz: $(SOURCES)
	@tar -czvf $(NAME).tgz Makefile $(SOURCES) $(OTHER)

clean:
	@echo Cleaning up...
	@rm -f $(JUNK)
