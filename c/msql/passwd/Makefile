CC=cc
LD=gcc
CFLAGS=-I/usr/people/dustin/lib/msql/include -O2
LDFLAGS=-L/usr/people/dustin/lib/msql/lib -lmsql

all: clean genauthdb rmauthdb

genauthdb: genauthdb.o
	ci -l -mcompiling genauthdb.c
	$(CC) -o genauthdb genauthdb.o $(LDFLAGS)

rmauthdb: rmauthdb.o
	ci -l -mcompiling rmauthdb.c
	$(CC) -o rmauthdb rmauthdb.o $(LDFLAGS)

install: genauthdb rmauthdb
	strip genauthdb rmauthdb
	chown root.wheel genauthdb rmauthdb
	chmod 4750 genauthdb
	chmod 750 rmauthdb
	mv genauthdb rmauthdb /usr/local/sbin

clean:
	rm -f genauthdb.o genauthdb genauthdb.core
