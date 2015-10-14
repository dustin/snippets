CFLAGS=-Wall -Werror -I -g

msptest: msp.o msptest.o
	$(CC) -g -o $@ msp.o msptest.o

msp.o: msp.h msp.c Makefile
	$(CC) $(CFLAGS) -o $@ -c msp.c

msptest.o: msp.h msptest.c  Makefile
