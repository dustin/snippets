/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: hash.h,v 1.5 2000/07/30 07:41:56 dustin Exp $
 */

#ifndef HASH_H
#define HASH_H 1

#include <stdlib.h>
#include <string.h>
#ifdef USE_PTHREAD
#include <pthread.h>
#endif /* USE_PTHREAD */

struct hash_container {
	unsigned int    key;
	unsigned int    in;
	unsigned int    out;
	struct hash_container *next;
};

struct hash_keylist {
	int  nentries;
	int *entries;
};

struct hashtable {
	int     hashsize;
#ifdef USE_PTHREAD
	pthread_mutex_t *mutexen;
#endif /* USE_PTHREAD */
	struct hash_container **buckets;
};

struct hashtable *hash_init(int size);
struct hash_container *hash_store(struct hashtable *hash, unsigned int key);
struct hash_container *hash_add(struct hashtable *hash,
	unsigned int key, int in, int out);
struct hash_container *hash_find(struct hashtable *hash, unsigned int key);
void    hash_delete(struct hashtable *hash, unsigned int key);
void    hash_destroy(struct hashtable *hash);
void    _hash_dump(struct hashtable *hash);
struct hash_keylist hash_keys(struct hashtable *hash);

#endif /* HASH_H */
