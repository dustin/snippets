#include <stdio.h>
#include <ctype.h>
#include <assert.h>

#include "scrabble.h"

/* All return values from originalScoreLetter */
int scores[]={
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1,
	3, 3, 2, 1, 4, 2, 4, 1, 8, 5, 1, 3, 1, 1, 3, 10, 1, 1, 1, 1, 4, 4,
	8, 4, 10, -1, -1, -1, -1, -1, -1, 1, 3, 3, 2, 1, 4, 2, 4, 1, 8, 5,
	1, 3, 1, 1, 3, 10, 1, 1, 1, 1, 4, 4, 8, 4, 10, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
};

int
originalScoreLetter(char c)
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

	/* i, at this point, is the length of the string */
	if(i==8) {
		rv+=50;
	}

	return(rv);
}

#ifdef SCORING_MAIN

int
main(int argc, char**argv)
{
	int i=0;
	printf("Score of %s is %d\n", argv[1], scoreWord(argv[1]));

	printf("Verifying scoring algorithm.\n");
	for(i=0; i<255; i++) {
		if(scoreLetter(i) != originalScoreLetter(i)) {
			printf("Value not equal for %d\n", i);
		}
	}
	printf("Scoring algorithm verified.\n");

	return(0);
}

#endif /* SCORING_MAIN */
