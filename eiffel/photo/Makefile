E=smalleiffel
EC=$(E) compile

RPATH=-rpath /usr/local/pgsql/lib

EFLAGS=-g -I/usr/local/pgsql/include
LFLAGS=-L/usr/local/pgsql/lib $(RPATH) -lpq -lgen
CSTUFF=/home/dustin/prog/eiffel/pg/pg_cstuff.c

getpic: getpic.e
	$(EC) $@ -o $@ $(EFLAGS) $(CSTUFF) $(LFLAGS)

test: test.e photo.e
	$(EC) $@ -o $@ $(EFLAGS) $(CSTUFF) $(LFLAGS)

clean:
	$(E) clean getpic test
	rm -f getpic test
