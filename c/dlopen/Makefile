LDFLAGS=# -lc
EXES=dltest
CFLAGS=-fPIC
JUNK=libtest.so *.o $(EXES) *~ *.core

all: dltest libtest

dltest: dltest.o
	$(CC) -o $@ dltest.o $(LDFLAGS)

libtest: libtest.o
	ld -x -shared -o libtest.so libtest.o

clean:
	rm -f $(JUNK)
