INCDIR = /usr/src/Mesa/include

XLIBS=-L/usr/X11R6/lib -lX11 -lXext -lXmu
XLIBS2=-L/usr/X11R6/lib -lX11 -lXext

GL_LIBS = -lglut -lGLU -lGL -lm $(XLIBS)
GL_LIBS2 = -laux -ltk -lGLU -lGL -lm $(XLIBS2)

LIB_DEP =
CFLAGS = -O2
PROGS = generate generate2 model

all:	$(PROGS)

generate: generate.o
	$(CC) -o generate generate.o

generate2: generate2.o
	$(CC) -o generate2 generate2.o

model: model.o data.o
	$(CC) -o $@ model.o data.o $(GL_LIBS)

clean:
	-rm -f *.o *~ $(PROGS)
