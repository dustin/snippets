/*
 * Copyright (c) 1998 Beyond.com
 * Written by Dustin Sallings <dustin@beyond.com>
 *
 * $Id: mmmain.c,v 1.2 1998/11/04 07:27:26 dustin Exp $
 */

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <pcre.h>

#include "hash.h"
#include "mymalloc.h"

#undef USE_MMAP

#define WORDMAP "fixupmap"

#ifndef MAP_FILE
#define MAP_FILE 0   /* for non-BSD systems */
#endif

/* this is kinda like strrchr, 'cept it doesn't start at the end of the
 * line
 */

static char *findcharr(char *base, char *string, int c)
{
	/* Search backwards from the string point, but not before the beginning
	 * of the string.
	 */
	while( (string>=base) && (*string!=c)) string--;

	if(string<base)
	    string=base;

    return(string);
}

/* split on a character */
static char **split(char c, char *string)
{
	int i, j=0, length;
	char **ret;  /* what we return */
	char *p, *s;

	s=strdup(string);
	length=strlen(s);

	p=s+length-1;

	/* count how many things we've got */
	for(i=0;i<length;i++) {
		if(s[i]==c) {
			s[i]=0x00;
			j++;
		}
	}

	/* j++; */

	/* Malloc the return */
	ret = (char **) calloc((j + 2), sizeof(char *));

	for (; j >= 0; j--) {
		while(*p && p>=s)
			p--;
		ret[j]=strdup(p+1);
		p--;
	}

	free(s);
	return(ret);
}

/* free a char **, like one returned from the above routine */
static void freeptrlist(char **list)
{
    int     i;

    if (list == NULL)
        return;

    for (i = 0; list[i]; i++) {
        free(list[i]);
    }
    free(list);
}

/* convert spaces to .*? */
static char *_space_to_re(char *subject)
{
	char **s, *ret;
	int i, max, size=0;

	s=split(' ', subject);

	for(max=0; s[max]; max++)
		size+=strlen(s[max]);

	ret=calloc(1, ( (sizeof(char *)*size) + ( (sizeof(char *)*3*max) ) + 1 ));
	assert(ret);

	/* get the beginning */
	strcpy(ret, s[0]);

	for(i=1; i<max; i++) {
		strcat(ret, ".*?");
		strcat(ret, s[i]);
	}

	freeptrlist(s);
	return(ret);
}

/* get hash for word map */
static struct hashtable *_get_wordmap(void)
{
	struct hashtable *h;
	FILE *f;
	char buf[1024], *p;

	h=hash_init(HASHSIZE);
	assert(h);

	f=fopen(WORDMAP, "r");

	while( fgets(buf, 1024, f)!=NULL ) {
		buf[strlen(buf)-1]=0x00;
		p=strchr(buf, ',');
		*p=0x00;
		p++;
		hash_store(h, buf, p);
	}

	fclose(f);
	return(h);
}

static char *_translate_re(char *subject)
{
	char **s, *ret;
	int i, max, size=0;
	struct hashtable *h;
	struct hash_container *ent;

	h=_get_wordmap();

	s=split(' ', subject);

	for(max=0; s[max]; max++)
		size+=strlen(s[max]);

	ret=calloc(1, ( (sizeof(char *)*size) + ( (sizeof(char *)*3*max) ) + 1 ));
	assert(ret);

	/* get the beginning */
	ent=hash_find(h, s[0]);
	if(ent)
		strcat(ret, ent->value);
	else
		strcat(ret, s[0]);

	for(i=1; i<max; i++) {
		strcat(ret, " ");
		ent=hash_find(h, s[i]);
		if(ent)
			strcat(ret, ent->value);
		else
			strcat(ret, s[i]);
	}

	freeptrlist(s);
	hash_destroy(h);
	return(ret);
}

/* Figure out what the user wants, return in buf */
static char *_enhance_re(char *regex)
{
	char *tmp1, *tmp2;

	tmp1=_translate_re(regex);
	tmp2=_space_to_re(tmp1);

	/* set tmp1 to tmp2 and free the old tmp1 */
	free(tmp1); tmp1=tmp2; tmp2=NULL;

	return(tmp1);
}

int
main(int argc, char **argv)
{
	pcre   *pattern = NULL;
	pcre_extra *hints = NULL;
	const char *error;
	int     errptr, offsets[99], matches, fd, got=1, end;
	char    buf[1024];
	char    buf2[1024];
	char   *source, *p, *start, *stop, *optpat;
	struct stat st;

	assert(argc > 2);

	/* This isn't really necessary, and probably lowers performance, but we
	 * want to know what we got in case of a weird regex.  Should probably
	 * be a flag.
	 */

	optpat=_enhance_re(argv[1]);
	strcpy(buf, "(");
	strcat(buf, optpat);
	strcat(buf, ")");
	free(optpat);

	printf("Pattern is %s\n", buf);

    /* compile and study the pattern.  In some complicated regex patterns,
	 * the study now should improve performance when we go through a lot of
	 * iterations.
	 */
	pattern = pcre_compile(buf, PCRE_CASELESS, &error, &errptr, NULL);
	if (pattern == NULL) {
		fprintf(stderr, "pgrep: error in regex at offset %d: %s\n", errptr,
		    error);
		return 2;
	}
	hints = pcre_study(pattern, 0, &error);
	if (error != NULL) {
		fprintf(stderr, "pgrep: error while studing regex: %s\n", error);
		return 2;
	}

	/*
	 * matches sets the number of matches we're going to be able to extract
	 * from any given execution of the regex.  i.e. for something like
	 * (.*), we get one, for (\d+).*?(\d+) we get two, etc...
	 */
	matches = pcre_info(pattern, NULL, NULL);

	printf("Matches is %d\n", matches);

    /* Stat to find out how much to mmap, and open the file */
	stat(argv[2], &st);
	fd = open(argv[2], O_RDONLY, 0);
	assert(fd);

/* Map it, or malloc/read/free */
#ifdef USE_MMAP
	source = mmap(NULL, st.st_size, PROT_READ, MAP_FILE|MAP_SHARED, fd, 0);
	assert(source);
#else
	source = malloc(st.st_size);
	read(fd, source, st.st_size);
	close(fd);
#endif

	/* Loop through matches, p is our current position, end is the end of
	 * the memory, so we can know how much is ours to search. */
    p=source;
	end=(int)p+st.st_size;
	while (got && *p) {
		if (got = pcre_exec(pattern, hints,
			p, (int)end-(int)p, 0, offsets, 99) >= 0) {

			/* this if probably shouldn't be here.  We put parens around
			 * the regex, so it's at least 1 */
			if (matches > 0) {
				printf("\tMatched %d bytes (%d %d)\n", offsets[3] - offsets[2],
				    offsets[2], offsets[3]);

				/* find beginning of the line */
				start=findcharr(source, p+offsets[2], '\n');
				start++;

				/* find end of the line */
				stop=strchr(p+offsets[2], '\n');

				/* Show what the pattern found */
				strncpy(buf2, p+offsets[2], offsets[3]-offsets[2]);
				buf2[offsets[3]-offsets[2]]=0x00;
				printf("\tPattern matched %s\n", buf2);

				/* Show the line we matched */
				strncpy(buf2, start, (int)stop-(int)start);
				buf2[(int)stop-(int)start] = 0x00;
				printf("\tLine Matched:  %s\n", buf2);
			}
		}
		p=stop+1; /* move current position to the next line */
	}

#ifdef USE_MMAP
	munmap(source, st.st_size);
#else
	free(source);
#endif

#ifdef MYMALLOC
	puts("");
	_mdebug_dump();
#endif

}
