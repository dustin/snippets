#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "scrabble.h"

/* Returns the number of letters required to spell the word */
int
wordMatches(const char *letters, const char *word)
{
	int letterMatches=0;
	int i=0, j=0;
	int used[8];

	assert(letters);
	assert(word);

	memset(&used, 0x00, sizeof(used));

	letterMatches=0;
	for(i=0; word[i]!=NULL; i++) {
		for(j=0; letters[j]!=NULL; j++) {
			if(word[i]==letters[j] && used[j]==0) {
				letterMatches++;
				used[j]=1;
			}
		}
	}

	/* i = strlen(word), letterMatches == number of letters matched */

	return(i-letterMatches);
}

char **
getMatches(const char *letters, char **dict)
{
	char **rv=NULL;
	int size=0, current=0, i=0;

	LINIT(rv, current, size);

	for(i=0; dict[i]; i++) {
		if(wordMatches(letters, dict[i]) <2) {
			LAPPEND(rv, current, size, dict[i]);
		}
	}

	return(rv);
}

#ifdef SEARCH_MAIN
int
main(int argc, char **argv)
{
	char **list;
	char **dict;
	int i=0;

	dict=loadDict(argv[1]);
	assert(dict);

	list=getMatches(argv[2], dict);
	assert(list);

	for(i=0; list[i]; i++) {
		printf("%d\t%s\n", scoreWord(list[i]), list[i]);
	}

	freeWordList(list);
	freeWordList(dict);

	return(0);
}
#endif /* SEARCH_MAIN */
