E=se
EC=$(E) compile

RPATH=-R /usr/local/pgsql/lib

EFLAGS=-g -I/usr/local/pgsql/include
# LFLAGS=-L/usr/local/pgsql/lib $(RPATH) -lpq
LFLAGS=-L/usr/local/pgsql/lib -lpq

FILES=Makefile pgtest.e pg.e pg_cstuff.c pg_replicate.e

pgtest: pgtest.e pg.e pg_cstuff.c
	$(EC) pgtest -o $@ $(EFLAGS) pg_cstuff.c $(LFLAGS)

temploader: temploader.e pg.e pg_cstuff.c
	$(EC) temploader.e -o $@ $(EFLAGS) pg_cstuff.c $(LFLAGS)

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
