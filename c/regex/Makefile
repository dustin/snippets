CFLAGS=-I. -g -DMYMALLOC

all: mwoo makeindex

mwoo: mmmain.o mymalloc.o hash.o
	$(CC) -o $@ mmmain.o mymalloc.o hash.o -L. -lpcre

makeindex: makeindex.o
	$(CC) -o $@ makeindex.o
