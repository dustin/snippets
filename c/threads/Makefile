CFLAGS=
LDFLAGS=-lpthread

.c.o: $<
	$(CC) $(CFLAGS) -o $@ $<

.o.: $<
	$(CC) -o $@ $< $(LDFLAGS)
