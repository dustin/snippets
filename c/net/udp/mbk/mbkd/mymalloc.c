
/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: mymalloc.c,v 1.2 1998/10/03 08:20:27 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <limits.h>
#include <assert.h>

/* #define NUM_MEMBUCKETS 16369 */
/* #define NUM_MEMBUCKETS 24571 */
#define NUM_MEMBUCKETS 12289

#ifdef MYMALLOC
static struct memories {
	char   *p;
	size_t  size;
	struct memories *next;
	char   *file;
	int     line;
}     **memories = NULL;

static int *_mem_stats = NULL;

void    _mdebug_dump(void);

static int 
_getmemindex(void *p)
{
	return ((int) p % NUM_MEMBUCKETS);
}

static void 
_register_mem(void *p, size_t size, char *file, int line)
{
	struct memories *m, *c;
	int     index;

	assert(p);
	m = malloc(sizeof(struct memories));
	assert(m);
	m->p = p;
	m->size = size;
	m->next = NULL;
	m->file = file;
	m->line = line;

	if (memories == NULL) {
		memories = calloc(NUM_MEMBUCKETS, sizeof(struct memories *));
		assert(memories);
		_mem_stats = calloc(NUM_MEMBUCKETS, sizeof(int));
		assert(_mem_stats);
	}
	index = _getmemindex(p);

	/* Gather statistics */
	_mem_stats[index]++;

	c = memories[index];

	if (c == NULL) {
		memories[index] = m;
	} else {
		for (; c->next != NULL; c = c->next);
		c->next = m;
	}
}

void   *
_lookup_mem(void *p)
{
	struct memories *c;
	int     index;
	index = _getmemindex(p);
	for (c = memories[index]; c && c->p != p; c = c->next);
	return (c);
}

static void 
_unregister_mem(void *p)
{
	struct memories *c, *tmp;
	int     index;

	index = _getmemindex(p);

	/* Special case for first thingy */
	if (memories[index]->p == p) {
		if (memories[index]->next) {
			tmp = memories[index]->next;
			free(memories[index]);
			memories[index] = tmp;
		} else {
			free(memories[index]);
			memories[index] = NULL;
		}
	} else {
		for (c = memories[index]; c && c->next->p != p; c = c->next);
		assert(c);
		tmp = c->next;
		c->next = c->next->next;
		free(tmp);
	}
}

void 
_mdebug_dump(void)
{
	struct memories *c;
	int     i, count = 0, min, max, avg, empty;

	for (i = 0; i < NUM_MEMBUCKETS; i++) {
		for (c = memories[i]; c; c = c->next) {
			printf("Found memory at %d (%p)\n", i, (void *) c);
			count++;
			printf("MEM:  %p is %d bytes (%p)\n", (void *) c->p, c->size,
			    (void *) c->p);
			printf("\t%s line %d\n", c->file, c->line);
		}
	}

	if (count == 0)
		printf("No registered memory\n");

	max = 0;
	min = INT_MAX;
	avg = 0;
	empty = 0;
	for (i = 0; i < NUM_MEMBUCKETS; i++) {
		if (_mem_stats[i] == 0)
			empty++;
		if (_mem_stats[i] > max)
			max = _mem_stats[i];
		if (_mem_stats[i] < min)
			min = _mem_stats[i];
		avg += _mem_stats[i];
	}

	printf("Hash size was %d buckets\n"
	    "Highest:  %d\n"
	    "Lowest:   %d\n"
	    "Average:  %d\n"
	    "Empty:    %d\n",
	    NUM_MEMBUCKETS, max, min, avg / NUM_MEMBUCKETS, empty);
}

void 
_mdebug_long_stats(void)
{
	int     i;
	for (i = 0; i < NUM_MEMBUCKETS; i++) {
		if (_mem_stats[i] > 0)
			printf("MEM:  %d got %d hits\n", i, _mem_stats[i]);
	}
}

void   *
_my_malloc(size_t size, char *file, int line)
{
	void   *p;
	p = malloc(size);
	assert(p);
	_register_mem(p, size, file, line);
	return (p);
}

void   *
_my_calloc(size_t n, size_t size, char *file, int line)
{
	void   *p;
	p = calloc(n, size);
	assert(p);
	_register_mem(p, size * n, file, line);
	return (p);
}

char   *
_my_strdup(char *str, char *file, int line)
{
	char   *p;
	p = strdup(str);
	_register_mem(p, strlen(p), file, line);
	return (p);
}

void   *
_my_realloc(void *p, size_t size, char *file, int line)
{
	void   *ret;
	assert(_lookup_mem(p));
	_unregister_mem(p);
	ret = realloc(p, size);
	assert(ret);
	_register_mem(ret, size, file, line);
	return (ret);
}

void 
_my_free(void *p, char *file, int line)
{
	void   *tmp;
	tmp = _lookup_mem(p);
	if (tmp == NULL) {
		printf("Trying to free something that isn't mine:  %p (%s) (%s:%d)\n",
		    p, (char *) p,
		    file, line);
		_mdebug_dump();
		abort();
	}
	_unregister_mem(p);
	free(p);
}

#endif /* MYMALLOC */
