CFLAGS=-g
RM=rm -f

test: radius.o md5.o test.o
	$(CC) -o $@ radius.o md5.o test.o

clean:
	$(RM) test radius.o md5.o test.o
