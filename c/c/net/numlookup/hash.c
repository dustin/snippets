/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: hash.c,v 1.3 1999/05/08 22:21:58 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "hash.h"
#include "mymalloc.h"

/* We don't need a function for this */
#define _do_hash(a, b) (b%a->hashsize)

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
    int key, void *value)
{
	struct hash_container *c, *p;
	int     hashval;

	c = calloc(1, sizeof(struct hash_container));
	assert(c);

	c->key = key;

	c->value = strdup(value);
	c->next = NULL;

	hashval = _do_hash(hash, key);

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
hash_find(struct hashtable *hash, int key)
{
	struct hash_container *p;
	int     hashval;

	hashval = _do_hash(hash, key);

	p = hash->buckets[hashval];

	for (; p; p = p->next) {
		if (p->key==key)
			break;
	}

	return (p);
}

/* Delete an entry from the hash table */
void
hash_delete(struct hashtable *hash, int key)
{
	struct hash_container *p, *deleteme = NULL;
	int     hashval;

	hashval = _do_hash(hash, key);
	p = hash->buckets[hashval];

	for (; p->next; p = p->next) {
		if (p->next->key==key)
			break;
	}

	if (!p->next) {
		/* Stopped for a reason other than a match, rewind the bucket, and
		 * check the first key */
		p = hash->buckets[hashval];
		if (p->key==key) {
			deleteme = p;
			hash->buckets[hashval] = p->next;
		}
	} else {
		deleteme = p->next;
		p->next = p->next->next;
	}

	if (deleteme) {
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

	printf("Hash dump for hash at %p, size is %d:\n", (void *)hash,
		hash->hashsize);

	for (i = 0; i < hash->hashsize; i++) {
		p = hash->buckets[i];

		if (p) {
			for (; p; p = p->next) {
				printf("\t%d=%s\n", p->key, (char *) p->value);
			}
		}
	}
}
