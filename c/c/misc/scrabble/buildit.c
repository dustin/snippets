#include <stdio.h>

#include "scrabble.h"

int
main(int argc, char **argv)
{
	int i=0;

	for(i=0; i<256; i++) {
		printf("%d, ", scoreLetter((char)i));
	}

	return(0);
}
