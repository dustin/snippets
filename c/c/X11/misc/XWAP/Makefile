CFLAGS=-O6 -m486 -s -Wall -I/usr/X11R6/include
LDFLAGS=-s -L/usr/X11R6/lib
CC=cc
LIBS= $(LDFLAGS) -lX11
ATHENA_LIBS=lXaw3d -lXmu -lXt -lXext -lX11
MOTIF_LIBS=-lXm -lXt -lX11
TOOLKIT_APPS=athena motif
R6_APPS=chapr4cms

all: chap1 chap2 chap3 chap4 chap5 chap6 chap7 chap8 chap9 chap10 \
	chap11 chap12 chap13 drawapp $(TOOLKIT_APPS) $(R6_APPS)

connect.o: connect.c
	$(CC) $(CFLAGS) -c connect.c

chap1.o: chap1.c

chap1: chap1.o connect.o
	$(CC) $(CFLAGS) -o chap1 chap1.o connect.o $(LIBS)

classhnt.o:	classhnt.c
	$(CC) $(CFLAGS) -c classhnt.c

sizehint.o:	sizehint.c
	$(CC) $(CFLAGS) -c sizehint.c

window.o:	window.c
	$(CC) $(CFLAGS) -c window.c

wmhints.o:	wmhints.c
	$(CC) $(CFLAGS) -c wmhints.c

wmname.o:	wmname.c
	$(CC) $(CFLAGS) -c wmname.c

chap2.o:	chap2.c
	$(CC) $(CFLAGS) -c chap2.c


chap2:	chap2.o classhnt.o connect.o sizehint.o window.o wmhints.o wmname.o
	$(CC) $(CFLAGS) -o chap2 chap2.o wmhints.o wmname.o classhnt.o \
	connect.o sizehint.o window.o $(LIBS)

gc.o:	gc.c
	$(CC) $(CFLAGS) -c gc.c

oval.o:	oval.c
	$(CC) $(CFLAGS) -c oval.c

chap3.o:	chap3.c
	$(CC) $(CFLAGS) -c chap3.c

chap3:	chap3.o gc.o oval.o classhnt.o connect.o sizehint.o window.o \
		wmhints.o wmname.o
	$(CC) $(CFLAGS) -o chap3 chap3.o gc.o oval.o wmhints.o wmname.o \
		classhnt.o connect.o sizehint.o window.o $(LIBS)

color.o: color.c
	$(CC) $(CFLAGS) -c color.c

colormap.o: colormap.c
	$(CC) $(CFLAGS) -c colormap.c

visual.o: visual.c
	$(CC) $(CFLAGS) -c visual.c

chap4.o: chap4.c
	$(CC) $(CFLAGS) -c chap4.c

chap4: chap4.o color.o colormap.o gc.o oval.o visual.o wmhints.o \
	wmname.o classhnt.o connect.o sizehint.o window.o
	$(CC) $(CFLAGS) -o chap4 chap4.o color.o colormap.o gc.o \
		oval.o visual.o classhnt.o connect.o sizehint.o \
		window.o wmhints.o wmname.o $(LIBS)
tgz:
	@tar -czvf X11.tgz Makefile *.c

clean:
	@echo Cleaning up...
	@rm -f *.o core chap? chap1?
