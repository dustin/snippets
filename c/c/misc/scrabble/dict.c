#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "scrabble.h"

static int
dictcompare(const void *a, const void *b)
{
	char *ac=NULL, *bc=NULL;
	int scorea=0, scoreb=0;
	int rv=0;

	assert(a!=NULL);
	assert(b!=NULL);

	ac=(char *)*((char **)a);
	bc=(char *)*((char **)b);

	scorea=scoreWord(ac);
	scoreb=scoreWord(bc);

	if(scorea>scoreb) {
		rv=-1;
	} else if (scorea==scoreb) {
		rv=0;
	} else {
		rv=1;
	}

	return(rv);
}

char **
loadDict(const char *dict)
{
	char **rv=NULL;
	int size=32768;
	int current=0;
	FILE *f=NULL;
	char buf[8192];

	f=fopen(dict, "r");
	if(f==NULL) {
		perror(dict);
		return(NULL);
	}

	LINIT(rv, current, size);

	while( (fgets(buf, sizeof(buf), f)) != NULL) {
		buf[strlen(buf)-1]=0x00;
		if(strlen(buf)>1 && strlen(buf)<9 && islower(buf[0])) {
			LAPPEND(rv, current, size, buf);
		} else {
			/* fprintf(stderr, "REJECTING %s\n", buf); */
		}
	}
	fclose(f);

	qsort(rv, current, sizeof(char *), dictcompare);

	return(rv);
}

void
freeWordList(char **list)
{
	int i=0;
	for(i=0; list[i]; i++) {
		free(list[i]);
	}
	free(list);
}

#ifdef DICT_MAIN

int
main(int argc, char **argv)
{
	char **dict=NULL;
	int i=0;

	dict=loadDict(argv[1]);
	assert(dict);

	printf("Loaded, printing.\n");

	for(i=0; dict[i]; i++) {
		printf("%d\t%s\n", scoreWord(dict[i]), dict[i]);
	}

	freeWordList(dict);

	return(0);
}

#endif /* DICT_MAIN */
