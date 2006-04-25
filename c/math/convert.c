/*
 * Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <assert.h>

/* Allocate some more memory */
inline static void adjustUp(int *size, char **ptr) {
	*size += 32;
	assert(*size > 0);
	*ptr=realloc(*ptr, *size);
	assert(*ptr != NULL);
}

static void rev(char *in) {
	assert(in != NULL);
	char tmp=0x00;
	int i=0;
	int len=strlen(in);

	for(i=0; i<len/2; i++) {
		int lpos=0, rpos=0;
		lpos=i;
		rpos=len-i-1;
		tmp=in[rpos];
		in[rpos]=in[lpos];
		in[lpos]=tmp;
	}
}

/* This is not meant to be efficient */
char *convertBase(char *in, int base1, int base2) {
	static char *digits="0123456789abcdef";
	static char *rv=NULL;
	static int allocated=0;
	int val=0, i=0, j=0;

	assert(in != NULL);
	assert(base1 >= 2);
	assert(base1 <= 16);
	assert(base2 >= 2);
	assert(base2 <= 16);

	/* Figure out what the input value is */
	for(i=0; i<strlen(in); i++) {
		int thisval=0;
		char *p=NULL;
		assert(in[i] != 0);
		p=index(digits, in[i]);
		assert(p != 0);
		thisval=p-digits;
		assert(thisval < base1);

		val *= base1;
		val += thisval;

		assert(val >= 0);
	}

	/* Do the output */
	while(val >= base2) {
		if(j >= allocated) {
			adjustUp(&allocated, &rv);
		}
		rv[j++]=digits[val % base2];
		val/=base2;
	}
	if(j >= allocated) {
		adjustUp(&allocated, &rv);
	}
	rv[j++]=digits[val];

	/* NULL termination */
	rv[j]=0;
	/* Reverse it, since we built it backwards */
	rev(rv);

	return rv;
}

int main(int argc, char **argv) {
	int rv=0;
	if(argc < 4) {
		fprintf(stderr, "Usage:  %s num ibase obase\n", argv[0]);
		rv=1;
	} else {
		printf("%s\n", convertBase(argv[1], atoi(argv[2]), atoi(argv[3])));
	}

	return rv;
}
