E=smalleiffel
EC=$(E) compile

EFLAGS=-g -I/usr/local/pgsql/include -funsigned-char
LFLAGS=-L/usr/local/pgsql/lib -lpq
CSTUFF=/home/dustin/prog/eiffel/pg/pg_cstuff.c

getpic: getpic.e
	$(EC) $@ -o $@ $(EFLAGS) $(CSTUFF) $(LFLAGS)

clean:
	$(E) clean getpic
	rm -f getpic
