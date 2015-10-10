CXXFLAGS=-Wall -Werror -I

msptest: msp.o msptest.o
	$(CC) -o $@ msp.o msptest.o

msp.o: msp.h msp.c Makefile
	$(CC) -o $@ -c msp.c

msptest.o: msp.h msptest.c  Makefile
