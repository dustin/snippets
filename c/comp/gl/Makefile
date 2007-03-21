# -----------------------------------------------------------------
#
# This Makefile builds our executable for our "bubble sort" animiation.
#
# By : Wayne Brown    2-7-95
#
# -----------------------------------------------------------------
#
# Setup variables for the locations of important files and libraries
#

XLIB = -lXmu -lXext -lX11 

DEP_LIBRARIES = /usr/people/sallings/lib/libglut.a

LIBRARIES = $(DEP_LIBRARIES) $(XLIB) -lGL -lGLU # -lm

INCLUDES = -I. -I/usr/people/sallings/include -I/GL

OPTIONS = -O2 -xansi -Wf,-XNh2000 -call_shared

LDLIBS = -nostdlib -L/lib -L/usr/lib

OBJS = map.o

# ----------------------------------------------------------------
# Actually build the application based on the files that have been
# modified since the last "make"
#

map:	   map.c
	cc -o map map.c $(INCLUDES) $(OPTIONS) $(LIBRARIES) $(LDLIBS)

bike:	   bike.c
	cc -o bike bike.c -s $(INCLUDES) $(OPTIONS) $(LIBRARIES) $(LDLIBS)

bezmesh:   bezmesh.c
	cc -o bezmesh bezmesh.c -s $(INCLUDES) $(OPTIONS) $(LIBRARIES) $(LDLIBS)

# ----------------------------------------------------------------
# General purpose "functions" that are sometimes useful
#

all: 
	touch *.c *.h
	make

cleanup:
	rm *.o

