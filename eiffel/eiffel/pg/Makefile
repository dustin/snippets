E=smalleiffel
EC=$(E) compile

RPATH=-rpath /usr/local/pgsql/lib

EFLAGS=-g -I/usr/local/pgsql/include -debug_check
LFLAGS=-L/usr/local/pgsql/lib $(RPATH) -lpq

FILES=Makefile pgtest.e pg.e pg_cstuff.c pg_replicate.e

pgtest: pgtest.e pg.e pg_cstuff.c
	$(EC) pgtest -o $@ $(EFLAGS) pg_cstuff.c $(LFLAGS)

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
