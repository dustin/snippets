INCLUDES=
CFLAGS=-O2 $(INCLUDES)
LDFLAGS=-s

# if you have UFC lib installed, *HOOK* it up.  :)
UFC=/usr/people/dustin/lib/libufc.a

S_OBJS=sockets.o cracker.o newpasswd.o functions.o
C_OBJS=client.o
ST_OBJS=getstatus.o
SERV=cracker_serv
CLIENT=cracker_client
STAT=cracker_status
JUNK=*.o core $(SERV) $(CLIENT) $(STAT)

all: $(S_OBJS) $(C_OBJS) $(ST_OBJS)
	$(CC) -o $(SERV) $(LDFLAGS) $(S_OBJS)
	$(CC) -o $(CLIENT) $(LDFLAGS) $(C_OBJS) $(UFC)
	$(CC) -o $(STAT) $(LDFLAGS) $(ST_OBJS)

clean:
	rm -f $(JUNK)
