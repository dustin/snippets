CFLAGS=-g -DMYMALLOC

all: parselist libparselist.so

parselist: parselist.o
	$(CC) -o parselist parselist.o

libparselist.so: libparselist.o mymalloc.o
	rm -f libparselist.so
	$(LD) -x -shared -o libparselist.so libparselist.o mymalloc.o

clean:
	rm -f parselist parselist.o mymalloc.o
