E=smalleiffel
EC=$(E) compile

EFLAGS=-g -I/usr/local/pgsql/include
LFLAGS=-L/usr/local/pgsql/lib -lpq

pgtest: pg.e pg_cstuff.c
	$(EC) pg -o $@ $(EFLAGS) pg_cstuff.c $(LFLAGS)

short:
	$(E) short pg

clean:
	$(E) clean pg
	rm -f pgtest
