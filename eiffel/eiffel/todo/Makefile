E=smalleiffel
EC=$(E) compile

EFLAGS=-g -I/usr/local/pgsql/include
LFLAGS=-L/usr/local/pgsql/lib -rpath /usr/local/pgsql/lib/ -lpq
CSTUFF=/home/dustin/prog/eiffel/pg/pg_cstuff.c

todo: todo.e
	$(EC) $@ -o $@ $(EFLAGS) $(CSTUFF) $(LFLAGS)

clean:
	$(E) clean todo
	rm -f todo
