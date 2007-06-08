/*
 * Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

/* Convert a three character month to the numeric value */
static int parseMonth(const char *input) {
    int rv=-1;
	int inputInt=0;
	int i=0;

	for(i=0; i<4 && input[i]; i++) {
		inputInt = (inputInt << 8) | input[i];
	}

	switch(inputInt) {
		case 'Jan/': rv=0; break;
		case 'Feb/': rv=1; break;
		case 'Mar/': rv=2; break;
		case 'Apr/': rv=3; break;
		case 'May/': rv=4; break;
		case 'Jun/': rv=5; break;
		case 'Jul/': rv=6; break;
		case 'Aug/': rv=7; break;
		case 'Sep/': rv=8; break;
		case 'Oct/': rv=9; break;
		case 'Nov/': rv=10; break;
		case 'Dec/': rv=11; break;
	}

	return rv;
}

static void testMonthParsing() {
	char *months[] = {
		"Jan/", "Feb/", "Mar/", "Apr/", "May/", "Jun/",
		"Jul/", "Aug/", "Sep/", "Oct/", "Nov/", "Dec/"
	};
	int i=0, j=0;
	for(i=0; i<12; i++) {
		for(j=0; j<10; j++) {
			int rv=parseMonth(months[i]);
			if(i != rv) {
				fprintf(stderr, "Expected %d for %s, got %d\n",
					i, months[i], rv);
				abort();
			}
		}
	}
	for(j=0; j<10; j++) {
		for(i=0; i<12; i++) {
			int rv=parseMonth(months[i]);
			if(i != rv) {
				fprintf(stderr, "Expected %d for %s, got %d\n",
					i, months[i], rv);
				abort();
			}
		}
	}
}

int main(int argc, char **argv) {
	testMonthParsing();
	return 0;
}
