CFLAGS=-I/usr/local/include -I/usr/people/dustin/prog/c/net/udp/mbk/mbkd
generic: filter.o main.o
	cc -o filter filter.o main.o -lpcap \
		/usr/people/dustin/prog/c/net/udp/mbk/mbkd/libmbk.a
