E=smalleiffel compile

EFLAGS=-g -I/usr/local/pgsql/include
LFLAGS=-L/usr/local/pgsql/lib -lpq

pgtest: pg.e pg_cstuff.c
	$(E) pg -o $@ $(EFLAGS) pg_cstuff.c $(LFLAGS)

clean:
	smalleiffel clean pg
	rm -f pgtest
