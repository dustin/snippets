E=se
EC=$(E) compile

PG=/Users/dustin/local/pgsql-8.0.1
RPATH=-R $(PG)/lib

LOADPATH=$(HOME)/lib/se.loadpath
EFLAGS=-g -I$(PG)/include -loadpath $(LOADPATH)
CPPFLAGS=-I$(PG)/include
# LFLAGS=-L$(PG)/lib $(RPATH) -lpq
LFLAGS=-L$(PG)/lib -lpq

FILES=Makefile pgtest.e pg.e pg_cstuff.c pg_replicate.e

pgtest: pgtest.e pg.e pg_cstuff.o
	$(EC) pgtest -o $@ $(EFLAGS) pg_cstuff.o $(LFLAGS)

temploader: temploader.e pg.e pg_cstuff.o
	$(EC) temploader.e -o $@ $(EFLAGS) pg_cstuff.o $(LFLAGS)

dist: $(FILES)
	mkdir pg
	cp $(FILES) pg
	cp ../forum.txt pg
	tar cvf - pg | gzip -9vc > pg.tgz
	rm -rf pg

short:
	$(E) short pg

clean:
	$(E) clean pgtest
	rm -f pgtest
	$(E) clean temploader
	rm -f temploader
	rm pg_cstuff.o
