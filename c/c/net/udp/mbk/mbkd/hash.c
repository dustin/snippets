/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: hash.c,v 1.1 1998/10/01 17:04:46 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "hash.h"

/* The actual string hashing algorithm */
int
_do_hash(struct hashtable *hash, char *s)
{
	char   *p;
	unsigned int h = 0, g;

	for (p = s; *p; p++) {
		h = (h << 4) + (*p);
		if (g = h & 0xf0000000) {
			h = h ^ (g >> 24);
			h = h ^ g;
		}
	}
	return (h % hash->hashsize);
}

/* Initialize a hash table */
struct hashtable *
hash_init(int size)
{
	struct hashtable *hash;

	hash = malloc(sizeof(struct hashtable));
	assert(hash);

	hash->hashsize = size;

	hash->buckets = calloc(hash->hashsize, sizeof(struct hash_container *));
	assert(hash->buckets);

	return (hash);
}

/* Store something in a hash table */
struct hash_container *
hash_store(struct hashtable *hash,
    char *name, void *value)
{
	struct hash_container *c, *p;
	int     hashval;

	c = calloc(1, sizeof(struct hash_container));
	assert(c);

	c->name = strdup(name);
	assert(c->name);

	c->value = strdup(value);
	c->next = NULL;

	hashval = _do_hash(hash, name);

	p = hash->buckets[hashval];

	if (p) {
		for (; p->next; p = p->next);
		p->next = c;
	} else {
		hash->buckets[hashval] = c;
	}

	return (c);
}

/* find a key in a hash table */
struct hash_container *
hash_find(struct hashtable *hash, char *key)
{
	struct hash_container *p;
	int     hashval;

	hashval = _do_hash(hash, key);

	p = hash->buckets[hashval];

	for (; p; p = p->next) {
		if (strcmp(p->name, key) == 0)
			break;
	}

	return (p);
}

/* Delete an entry from the hash table */
void
hash_delete(struct hashtable *hash, char *key)
{
	struct hash_container *p, *deleteme = NULL;
	int     hashval;

	hashval = _do_hash(hash, key);
	p = hash->buckets[hashval];

	for (; p->next; p = p->next) {
		if (strcmp(p->next->name, key) == 0)
			break;
	}

	if (!p->next) {
		/* Stopped for a reason other than a match, rewind the bucket, and
		 * check the first key */
		p = hash->buckets[hashval];
		if (strcmp(p->name, key) == 0) {
			deleteme = p;
			hash->buckets[hashval] = p->next;
		}
	} else {
		deleteme = p->next;
		p->next = p->next->next;
	}

	if (deleteme) {
		if (deleteme->name)
			free(deleteme->name);
		if (deleteme->value)
			free(deleteme->value);
		free(deleteme);
	}
}

/* Destroy a hash */
void
hash_destroy(struct hashtable *hash)
{
	struct hash_container *p, *next;
	int     i;

	if (hash == 0)
		return;

	for (i = 0; i < hash->hashsize; i++) {
		p = hash->buckets[i];

		if (p) {
			for (; p;) {
				next = p->next;
				if (p->name)
					free(p->name);
				if (p->value)
					free(p->value);
				free(p);
				p = next;
			}
		}
	}
	if (hash->buckets)
		free(hash->buckets);
	free(hash);
}

/* debug stuff, dump the hash */
void
_hash_dump(struct hashtable *hash)
{
	struct hash_container *p;
	int     i;

	printf("Hash dump:\n");

	for (i = 0; i < hash->hashsize; i++) {
		p = hash->buckets[i];

		if (p) {
			for (; p; p = p->next) {
				printf("\t%s=%s", p->name, p->value);
			}
		}
	}
}
