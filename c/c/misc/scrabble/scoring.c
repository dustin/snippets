#include <stdio.h>
#include <ctype.h>
#include <assert.h>

#include "scrabble.h"

int
scoreLetter(char c)
{
	int rv=0;

	switch(toupper(c)) {
		case 'Q':
		case 'Z':
			rv=10;
			break;
		case 'J':
		case 'X':
			rv=8;
			break;
		case 'K':
			rv=5;
			break;
		case 'F':
		case 'H':
		case 'V':
		case 'W':
		case 'Y':
			rv=4;
			break;
		case 'B':
		case 'C':
		case 'M':
		case 'P':
			rv=3;
			break;
		case 'D':
		case 'G':
			rv=2;
			break;
		case 'A':
		case 'E':
		case 'I':
		case 'L':
		case 'N':
		case 'O':
		case 'R':
		case 'S':
		case 'T':
		case 'U':
			rv=1;
			break;
		case ' ':
			rv=0;
			break;
		default:
			fprintf(stderr, "UNKNOWN CHARACTER ``%c''\n", c);
			rv=-1;
	}
	return(rv);
}

int
scoreWord(const char *word)
{
	int rv=0;
	int i=0;
	assert(word);

	for(i=0; word[i]; i++) {
		int s=scoreLetter(word[i]);
		/* Only record positive scores */
		if(s>0) {
			rv+=s;
		}
	}

	if(strlen(word)==8) {
		rv+=50;
	}

	return(rv);
}

#ifdef SCORING_MAIN

int
main(int argc, char**argv)
{
	printf("Score of %s is %d\n", argv[1], scoreWord(argv[1]));
	return(0);
}

#endif /* SCORING_MAIN */
