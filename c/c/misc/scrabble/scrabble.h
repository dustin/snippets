#ifndef SCRABBLE_H
#define SCRABBLE_H 1

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "dict.h"

#ifndef BOOTSTRAP
extern int scores[];
#endif /* NOT BOOTSTRAP */

#define MAX_WORD_LEN 8

#define LINIT(list, current, size) \
	size=8192; \
	current=0; \
	list=calloc(sizeof(void *), size); \
	assert(list);

#define GENERIC_APPEND(list, current, size, dupfunction, newthing) \
	if(current==size-1) { \
		size+=8192; \
		list=realloc(list, size*sizeof(void *)); \
		assert(list); \
	} \
	list[current++]=dupfunction(newthing); \
	assert(list[current-1]); \
	list[current]=NULL;

#define LAPPEND(list, current, size, newthing) \
	GENERIC_APPEND(list, current, size, strdup, newthing)

#define DINIT LINIT

#define DAPPEND(list, current, size, newthing) \
	GENERIC_APPEND(list, current, size, dictDup, newthing)

struct match {
	char word[MAX_WORD_LEN+1];
	char needed_letters[MAX_WORD_LEN+1];
	short score;
	short nmissing;
};

/* Scoring */
int originalScoreLetter(char);
#ifdef BOOTSTRAP
# define scoreLetter originalScoreLetter
#else
# define scoreLetter(x) scores[(int)x]
#endif
int scoreWord(const char *);

/* Dictionary services */
#ifdef BOOTSTRAP
struct dict_entry **loadDict();
#endif /* BOOTSTRAP */

void freeDictEntry(struct dict_entry *freeMe);
void freeDictList(struct dict_entry **list);
void freeWordList(char **list);

/* Match stuff */
struct match **getMatches(const char *letters, int nletters);
void freeResultList(struct match **results);

#endif /* SCRABBLE_H */
