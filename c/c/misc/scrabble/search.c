#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "scrabble.h"

#define RAPPEND(list, current, size, newthing) \
	GENERIC_APPEND(list, current, size, dupResult, newthing)

/* Returns the number of letters required to spell the word */
struct match
wordMatches(const char *letters, struct dict_entry entry)
{
	int letterMatches=0;
	int i=0, j=0;
	int used[8];
	struct match rv;

	assert(letters);
	assert(strlen(entry.word)<MAX_WORD_LEN+1);

	memset(&used, 0x00, sizeof(used));
	memset(&rv, 0x00, sizeof(rv));

	strcpy(rv.word, entry.word);
	rv.score=entry.score;

	letterMatches=0;
	for(i=0; entry.word[i]!=NULL; i++) {
		int matched=0;
		for(j=0; matched==0 && letters[j]!=NULL; j++) {
			if(entry.word[i]==letters[j] && used[j]==0) {
				letterMatches++;
				used[j]=1;
				matched=1;
			}
		}
		if(matched==0) {
			rv.needed_letters[rv.nmissing++]=entry.word[i];
		}
	}

	/* i = strlen(word), letterMatches == number of letters matched */

	return(rv);
}

static struct match *
dupResult(struct match in)
{
	struct match *rv=NULL;

	rv=calloc(1, sizeof(in));
	assert(rv);

	memcpy(rv, &in, sizeof(in));

	return(rv);
}

void
freeResultList(struct match **results)
{
	int i=0;

	for(i=0; results[i]!=NULL; i++) {
		free(results[i]);
	}
	free(results);
}

struct match **
getMatches(const char *letters, int nletters)
{
	struct match **rv=NULL;
	int size=0, current=0, i=0;

	LINIT(rv, current, size);

	for(i=0; i<DICTIONARY_SIZE; i++) {
		struct match matchval;

		matchval=wordMatches(letters, dictionary[i]);
		if(matchval.nmissing <= nletters) {
			RAPPEND(rv, current, size, matchval);
		}
	}

	return(rv);
}

#ifdef SEARCH_MAIN
int
main(int argc, char **argv)
{
	struct match **list;
	int i=0;
	int nletters=1;

	if(argc>2) {
		nletters=atoi(argv[2]);
	}

	list=getMatches(argv[1], nletters);
	assert(list);

	switch(nletters) {
		case 0:
			printf("Exact matches:\n");
			break;
		case 1:
			printf("Results with one extra letter:\n");
			break;
		default:
			printf("Results with %d extra letters:\n", nletters);
			break;
	}

	for(i=0; list[i]; i++) {
		if(list[i]->nmissing>0) {
			printf("%d\t%s (need %s)\n", list[i]->score, list[i]->word,
				list[i]->needed_letters);
		} else {
			printf("%d\t%s\n", list[i]->score, list[i]->word);
		}
	}

	freeResultList(list);

	return(0);
}
#endif /* SEARCH_MAIN */
