CFLAGS=-Wall -I/usr/local/include -I/home/dustin/prog/c/net/udp/mbk/mbkd
generic: filter.o main.o netbsd.o
	cc -o filter filter.o main.o netbsd.o -lpcap \
		/home/dustin/prog/c/net/udp/mbk/mbkd/libmbk.a -lkvm
