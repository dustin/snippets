LDFLAGS=-lc
EXES=dltest
JUNK=libtest.so *.o $(EXES) *~

all: dltest libtest

dltest: dltest.o
	$(CC) -o $@ dltest.o $(LDFLAGS)

libtest: libtest.o
	ld -shared -o libtest.so libtest.o

clean:
	rm -f $(JUNK)
