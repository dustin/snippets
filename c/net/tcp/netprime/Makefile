CFLAGS=-g
SRC=netprime.c sockets.c utility.c client.c
SOBJ=netprime.o sockets.o utility.o
COBJ=client.o utility.o
EXES=netprime client

all: $(EXES)

netprime: $(SOBJ)
	$(CC) -o $@ $(SOBJ)

client: $(COBJ)
	$(CC) -o $@ $(COBJ)

clean:
	rm -f $(COBJ) $(SOBJ) $(EXES)
