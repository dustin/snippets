CFLAGS=-DMYMALLOC -DLIBNET_LIL_ENDIAN -I/usr/local/include -Wall
LDFLAGS=-L/usr/local/lib -lnids -lpcap -lnet

httplog: httplog.o mymalloc.o
	$(CC) -o $@ httplog.o mymalloc.o $(LDFLAGS)

httplog.o: httplog.c httplog.h
