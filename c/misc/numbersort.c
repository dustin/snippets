/*
 * Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

/* From my google interview.  Sort an arbitrarily long list of unique seven
 * digit phone numbers using less than 2MB of RAM.
 */

/* Phone number parser */
int parseNumber(const char *num)
{
	int rv=0, i=0, j=0;
	char *mutablenum=calloc(1, strlen(num)+1);
	assert(mutablenum!=0);

	for(i=0; i<strlen(num); i++) {
		if(isdigit(num[i])) {
			mutablenum[j++]=num[i];
		}
	}

	assert(strlen(mutablenum) > 0);

	return atoi(mutablenum);
}

#define BIT_GET(a, n) ((a[n/8] & (1 << (n % 8))) != 0)
#define BIT_SET(a, n) (a[n/8] |= (1 << (n % 8)))

/* Get phone numbers from stdin, print them back to stdout */
int main(int argc, char **argv)
{
	/* 1.5 MB buffer */
	unsigned char buf[1024*1536];
	char in[32];
	int i=0;

	memset(buf, 0x00, sizeof(buf));
	memset(in, 0x00, sizeof(in));

	/* Test my bit macros
	for(i=0; i<10000000; i++) {
		assert(BIT_GET(buf, i) == 0);
		BIT_SET(buf, i);
		assert(BIT_GET(buf, i) == 1);
	}
	*/

	while(fgets(in, sizeof(in), stdin) != NULL) {
		assert(strlen(in) > 0);
		assert(strlen(in) < sizeof(in));

		BIT_SET(buf, parseNumber(in));
	}

	/* Print them back out */
	for(i=0; i<10000000; i++) {
		if(BIT_GET(buf, i)) {
			printf("%07d\n", i);
		}
	}

}
