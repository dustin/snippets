/*
 * Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: hexdump.c,v 1.1 2000/10/05 00:12:11 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <ctype.h>
#include <assert.h>

/* Dump a block for debugging */
void dumpBlock(char *buffer, int size, int start)
{
	int i=0;
	int line=0;

	assert(buffer);

	printf("*** Dumping %d bytes ***\n", size);
	for(line=0; line*16<size; line++) {
		printf("%08X%03X  ", start, line*16);
		for(i=0; i<16; i++) {
			printf("%02X ", buffer[(line*16)+i]&0xff);
		}
		putchar(' ');
		for(i=0; i<16; i++) {
			int c=buffer[(line*16)+i];
			if(!iscntrl(c) && isascii(c) && (isgraph(c) || c==' ')) {
				putchar(c);
			} else {
				putchar('.');
			}
		}
		printf("\n");
	}
}

/* Dump a block in binary */
void binDumpBlock(char *buffer, int size, int start_addr)
{
	int i=0;
	printf("Dumping %d bytes in binary\n", size);
	for(i=0; i<size; i++) {
		printf("%04X (%02d) ", start_addr+i, i);
		printf("%s", buffer[i]&0x80?"1":"0");
		printf("%s", buffer[i]&0x40?"1":"0");
		printf("%s", buffer[i]&0x20?"1":"0");
		printf("%s", buffer[i]&0x10?"1":"0");
		printf("%s", buffer[i]&0x08?"1":"0");
		printf("%s", buffer[i]&0x04?"1":"0");
		printf("%s", buffer[i]&0x02?"1":"0");
		printf("%s", buffer[i]&0x01?"1":"0");
		puts("");
	}
}

int main(int argc, char **argv) {
	char buf[4096];
	int size=0, current=0;

	while( (size=read(0, &buf, sizeof(buf))) > 0) {
		dumpBlock(buf, size, current);
		current++;
	}
}
