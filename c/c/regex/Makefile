CFLAGS=-I. -O2 -g -DMYMALLOC
mmap: mmmain.o mymalloc.o hash.o
	$(CC) -o mwoo mmmain.o mymalloc.o hash.o -L. -lpcre
