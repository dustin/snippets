# Makefile for C binding network crap.

net: net.e sockets.c
	smalleiffel compile -o $@ $@ sockets.c

clean:
	smalleiffel clean net
	rm -f net sockets.o
