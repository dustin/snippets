PROFILE=-pg
CFLAGS=-O2 -g -Wall $(PROFILE)
LDFLAGS=-g
OBJ=scoring.o dict.o
LIB=-L. -lscrabble

dict: dict_main.o libscrabble.a
	$(CC) $(PROFILE) $(LDFLAGS) -o $@ dict_main.o $(LIB)

libscrabble.a: $(OBJ)
	ar rc $@ $(OBJ)
	ranlib libscrabble.a

dict_main.o: dict.c
	$(CC) $(CFLAGS) $(PROFILE) -DDICT_MAIN -c -o $@ dict.c

clean:
	rm -f libscrabble.a dict_main.o $(OBJ) dict
