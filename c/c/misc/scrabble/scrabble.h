#ifndef SCRABBLE_H
#define SCRABBLE_H 1

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

extern int scores[];

#define LINIT(list, current, size) \
	size=8192; \
	current=0; \
	list=calloc(sizeof(void *), size); \
	assert(list);

#define LAPPEND(list, current, size, newthing) if(current==size-1) { \
		size+=8192; \
		list=realloc(list, size*sizeof(char *)); \
		assert(list); \
	} \
	list[current++]=strdup(newthing); \
	assert(list[current-1]); \
	list[current]=NULL;

/* Scoring */
int originalScoreLetter(char);
/* int scoreLetter(char); */
#define scoreLetter(x) scores[(int)x]
int scoreWord(const char *);

/* Dictionary services */
char ** loadDict(const char *dict);
void freeWordList(char **list);

#endif /* SCRABBLE_H */
