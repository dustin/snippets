/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: hash.c,v 1.1 1998/06/10 08:41:59 dustin Exp $
 */

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <data.h>

int _do_hash(struct hashtable *hash, char *s)
{
    char *p;
    unsigned int h=0, g;

    for(p=s; *p; p++) {
	h=(h<<4)+(*p);
	if(g=h&0xf0000000) {
	    h=h^(g>>24);
	    h=h^g;
	}
    }
    return(h%hash->hashsize);
}

struct hashtable *hash_init(int size)
{
    struct hashtable *hash;

    hash=malloc(sizeof(struct hashtable));
    assert(hash);

    hash->hashsize=size;

    hash->buckets=calloc(hash->hashsize, sizeof(struct hash_container *));
    assert(hash->buckets);

    return(hash);
}

struct hash_container *hash_store(struct hashtable *hash,
				  char *name, void *value)
{
    struct hash_container *c, *p;
    int hashval;

    c=calloc(1, sizeof(struct hash_container));
    assert(c);

    c->name=strdup(name);
    assert(c->name);

    c->value=value;
    c->next=NULL;

    hashval=_do_hash(hash, name);

    p=hash->buckets[hashval];

    if(p) {
        for(;p->next;p=p->next);
	p->next=c;
    } else {
	hash->buckets[hashval]=c;
    }

    return(c);
}

struct hash_container *hash_find(struct hashtable *hash, char *key)
{
    struct hash_container *p;
    int hashval;

    hashval=_do_hash(hash, key);

    p=hash->buckets[hashval];

    for(;p;p=p->next) {
	if(strcmp(p->name, key)==0)
	    break;
    }

    return(p);
}
