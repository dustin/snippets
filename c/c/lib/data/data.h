/*
 * Copyright(c) 1998  Dustin Sallings
 *
 * $Id: data.h,v 1.1 1998/06/10 08:41:57 dustin Exp $
 */

/* Debug stuff */
#ifndef PDEBUG
#define PDEBUG 1
#endif

#if (PDEBUG>0)
# ifndef _ndebug
#  define _ndebug(a, b) if(PDEBUG > a ) printf b;
# endif
#endif

/* In case it didn't make it */
#ifndef _ndebug
#define _ndebug(a, b)
#endif

#define HASHSIZE 16369

struct hash_container {
    char *name;
    void *value;
    struct hash_container *next;
};

struct hashtable {
    int hashsize;
    struct hash_container **buckets;
};

int _do_hash(struct hashtable *hash, char *s);
struct hashtable *hash_init(int size);
struct hash_container *hash_store(struct hashtable *hash,
                                  char *key, void *data);
struct hash_container *hash_find(struct hashtable *hash, char *key);
