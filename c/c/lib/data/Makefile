CFLAGS=-I.
OBJS=hash.o

stats: $(OBJS) stats.o
	$(CC) -o $@ stats.o $(OBJS)

test: $(OBJS) test.o
	$(CC) -o $@ test.o $(OBJS)

doit: $(OBJS) doit.o
	$(CC) -o $@ doit.o $(OBJS)

clean:
	rm -f doit test stats test.o stats.o doit.o $(OBJS)
