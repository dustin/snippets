/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: hash.h,v 1.1 1998/11/04 05:09:16 dustin Exp $
 */

#ifndef HASH_H
#define HASH_H 1

#define HASHSIZE 16369

#include <stdlib.h>
#include <string.h>

struct hash_container {
	char   *name;
	char   *value;
	struct hash_container *next;
};

struct hashtable {
	int     hashsize;
	struct hash_container **buckets;
};

int     _do_hash(struct hashtable *hash, char *s);
struct hashtable *hash_init(int size);
struct hash_container *hash_store(struct hashtable *hash,
    char *key, void *data);
struct hash_container *hash_find(struct hashtable *hash, char *key);
void    hash_delete(struct hashtable *hash, char *key);
void    hash_destroy(struct hashtable *hash);
void    _hash_dump(struct hashtable *hash);
char **hash_keys(struct hashtable *hash);

#endif /* HASH_H */
