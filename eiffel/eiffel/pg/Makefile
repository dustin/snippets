E=smalleiffel
EC=$(E) compile

RPATH=-rpath /usr/local/pgsql/lib

EFLAGS=-g -I/usr/local/pgsql/include
LFLAGS=-L/usr/local/pgsql/lib $(RPATH) -lpq

pgtest: pgtest.e pg.e pg_cstuff.c
	$(EC) pgtest -o $@ $(EFLAGS) pg_cstuff.c $(LFLAGS)

short:
	$(E) short pg

clean:
	$(E) clean pgtest
	rm -f pgtest
