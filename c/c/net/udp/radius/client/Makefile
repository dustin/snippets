CFLAGS=-g -DMYMALLOC
RM=rm -f

test: radius.o md5.o test.o mymalloc.o
	$(CC) -o $@ radius.o md5.o test.o mymalloc.o

clean:
	$(RM) test radius.o md5.o test.o mymalloc.o
