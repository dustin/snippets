#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "scrabble.h"

struct dict_entry *
dictDup(struct dict_entry *in)
{
	struct dict_entry *rv=NULL;
	assert(in);

	rv=calloc(1, sizeof(struct dict_entry));
	assert(rv);

	rv->word=strdup(in->word);
	rv->score=in->score;

	return(rv);
}

static int
dictcompare(const void *a, const void *b)
{
	struct dict_entry *ac=NULL, *bc=NULL;
	int rv=0;

	assert(a!=NULL);
	assert(b!=NULL);

	ac=(struct dict_entry *)*((struct dict_entry **)a);
	bc=(struct dict_entry *)*((struct dict_entry **)b);

	if(ac->score>bc->score) {
		rv=-1;
	} else if (ac->score==bc->score) {
		rv=0;
	} else {
		rv=1;
	}

	return(rv);
}

static struct dict_entry *
makeDictEntry(const char *word)
{
	struct dict_entry *rv=NULL;

	assert(word);

	rv=calloc(1, sizeof(struct dict_entry));
	assert(rv);

	rv->word=strdup(word);
	rv->score=scoreWord(rv->word);

	return(rv);
}

void
freeDictEntry(struct dict_entry *freeMe)
{
	assert(freeMe);
	assert(freeMe->word);

	free(freeMe);
	free(freeMe->word);
}

#ifndef BOOTSTRAP
/*
struct dict_entry *
getDict()
{
	return &dictionary;
}
*/
#endif /* NOT BOOTSTRAP */

struct dict_entry **
loadDict()
{
	struct dict_entry **rv=NULL;
	int size=32768;
	int current=0;
	FILE *f=NULL;
	char buf[8192];

	f=fopen(DICTIONARY, "r");
	if(f==NULL) {
		perror(DICTIONARY);
		return(NULL);
	}

	DINIT(rv, current, size);

	while( (fgets(buf, sizeof(buf), f)) != NULL) {
		buf[strlen(buf)-1]=0x00;
		if(strlen(buf)>1 && strlen(buf)<9 && islower(buf[0])) {
			struct dict_entry *tmp=NULL;

			tmp=makeDictEntry(buf);
			DAPPEND(rv, current, size, tmp);
			freeDictEntry(tmp);

		} else {
			/* fprintf(stderr, "REJECTING %s\n", buf); */
		}
	}
	fclose(f);

	qsort(rv, current, sizeof(struct dict_entry *), dictcompare);

	return(rv);
}

void
freeDictList(struct dict_entry **list)
{
	int i=0;
	for(i=0; list[i]; i++) {
		freeDictEntry(list[i]);
	}
	free(list);
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
	struct dict_entry **dict=NULL;
	int i=0;

	dict=loadDict(argv[1]);
	assert(dict);

	printf("Loaded, printing.\n");

	for(i=0; dict[i]; i++) {
		printf("%d\t%s\n", dict[i]->score, dict[i]->word);
	}

	freeDictList(dict);

	return(0);
}

#endif /* DICT_MAIN */
