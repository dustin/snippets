/*
 * Copyright (c) 1997 Dustin Sallings
 *
 * $Id: utility.c,v 1.3 2000/01/18 02:54:01 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <assert.h>

#include "proxycgi.h"

/* grow a growstring */

void
str_append(struct growstring *s, char *buf)
{
	assert(s);
	assert(s->string);
	assert(buf);

	while ((strlen(s->string) + strlen(buf)) > s->size) {
		s->size += (1024 * (sizeof(char)));
		s->string = realloc(s->string, s->size);
		assert(s->string);
	}
	strcat(s->string, buf);
}

/* kill whitey, eat all the whitespace on the end of a string */

char   *
kw(char *in)
{
	assert(in);

	/* bounds checking */
	if (strlen(in) == 0)
		return (in);

	while (isspace(in[strlen(in) - 1])) {
		/* bounds checking */
		if (strlen(in) == 0)
			return (in);

		in[strlen(in) - 1] = 0x00;
	}

	return (in);
}

/* Yeah, there's probably a better way to do this, but I had this code, and
 * it worked at least at one time for me...
 */
char  **
split(char c, char *string)
{
	int     i, j = 0, length;
	char  **ret;
	char   *p;

	assert(string);

	length = strlen(string);

	p = string + length - 1;

	/* how many we got? */
	for (i = 0; i < length; i++) {
		if (string[i] == c) {
			string[i] = 0x00;
			j++;
		}
	}

	j++;

	ret = (char **) malloc((j + 1) * sizeof(char *));
	ret[j--] = NULL;

	for (; j >= 0; j--) {
		while (*p && p >= string)
			p--;
		ret[j] = strdup(p + 1);
		p--;
	}

	return (ret);
}

/* free a char **, like one returned from the above routine */
void
freeptrlist(char **list)
{
	int     i;

	if (list == NULL)
		return;

	for (i = 0; list[i]; i++) {
		free(list[i]);
	}
	free(list);
}

/* Ensure a path capable of writing the passed in file */
int
ensurepath(char *path)
{
	char **a;
	int i, rv, dotdot;
	char *ptmp;
	struct growstring grow;

	assert(path);
	ptmp=strdup(path);
	a=split('/', ptmp);

	/* Get rid of the last element */
	for(i=0; a[i]!=NULL; i++);
	free(a[i-1]);
	a[i-1]=NULL;

	grow.size=1024*sizeof(char);
	grow.string=calloc(sizeof(char), grow.size);
	assert(grow.string);

	dotdot=0;

	/* Get the first two pieces */
	str_append(&grow, "/");
	str_append(&grow, a[0]);
	if(strcmp(a[0], "..")==0) {
		dotdot+=2;
	}
	str_append(&grow, "/");
	str_append(&grow, a[1]);
	if(strcmp(a[1], "..")==0) {
		dotdot+=2;
	}
	rv=mkdir(grow.string, 0755);
	if(rv<0 && errno!=EEXIST) {
		return(-1);
	}

	for(i=2; a[i]!=NULL; i++) {
		str_append(&grow, "/");
		str_append(&grow, a[i]);
		if(strcmp(a[i], "..")==0) {
			dotdot+=2;
		}
		/* We want to make sure someone doesn't try to .. out of the
		 * controlled work area */
		assert(dotdot<i);
		rv=mkdir(grow.string, 0755);
		if(rv<0 && errno!=EEXIST) {
			return(-1);
		}
	}
	return(0);
}
