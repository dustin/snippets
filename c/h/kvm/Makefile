LIBS=-lkvm
LDFLAGS=$(LIBS)

COMMON_OBJ=kvm_interposition.o

hak: hak.o $(COMMON_OBJ)
	$(CC) -o $@ hak.o $(COMMON_OBJ) $(LDFLAGS)
