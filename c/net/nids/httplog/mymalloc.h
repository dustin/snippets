/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Orig: mymalloc.h,v 1.1 1998/06/22 00:07:33 dustin Exp $
 * $Id: mymalloc.h,v 1.1 2001/02/25 01:08:59 dustin Exp $
 */

#ifndef _MYMALLOC_H
#define _MYMALLOC_H 1

/* My memory management stuff */
#ifdef MYMALLOC
void _mdebug_dump(void);
void *_my_malloc(size_t size, char *file, int line);
void *_my_calloc(size_t n, size_t size, char *file, int line);
void *_my_realloc(void *p, size_t size, char *file, int line);
void _my_free(void *p, char *file, int line);
void *_mem_lookup(void *p, char *file, int line);
char *_my_strdup(char *str, char *file, int line);

#define malloc(a) _my_malloc(a, __FILE__, __LINE__)
#define calloc(a,b) _my_calloc(a, b, __FILE__, __LINE__);
#define realloc(a,b) _my_realloc(a, b, __FILE__, __LINE__)
#define strdup(a) _my_strdup(a, __FILE__, __LINE__)
#define free(a) _my_free(a, __FILE__, __LINE__)
#endif /* MYMALLOC */

#endif /* _MYMALLOC_H */
