INCDIR = /usr/src/Mesa/include

XLIBS=-L/usr/X11R6/lib -lX11 -lXext -lXmu
XLIBS2=-L/usr/X11R6/lib -lX11 -lXext

GL_LIBS = -lglut -lGLU -lGL -lm $(XLIBS)
GL_LIBS2 = -laux -ltk -lGLU -lGL -lm $(XLIBS2)

LIB_DEP =

CFLAGS = -O2

PROGS = bike

##### RULES #####

.SUFFIXES:
.SUFFIXES: .c

.c: $(LIB_DEP)
	$(CC) -I$(INCDIR) $(CFLAGS) $< $(GL_LIBS) -o $@



##### TARGETS #####

default:
	@echo "Specify a target configuration"

simple2:
	$(CC) -I$(INCDIR) $(CFLAGS) simple2.c $(GL_LIBS2) -o simple2

clean:
	-rm *.o *~

realclean:
	-rm $(PROGS)
	-rm *.o *~

targets: $(PROGS)
