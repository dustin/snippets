E=smalleiffel
EC=$(E) compile

EFLAGS=-g -I/usr/local/pgsql/include
LFLAGS=-L/usr/local/pgsql/lib -lpq

pgtest: pgtest.e pg.e pg_cstuff.c
	$(EC) pgtest -o $@ $(EFLAGS) pg_cstuff.c $(LFLAGS)

short:
	$(E) short pg

clean:
	$(E) clean pgtest
	rm -f pgtest
