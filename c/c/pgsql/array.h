/*
 * Copyright (c) 2002  Dustin Sallings
 *
 * $Id: array.h,v 1.1 2002/03/01 09:14:58 dustin Exp $
 */

#ifndef ARRAY_H
#define ARRAY_H 1

#include <sys/time.h>

#ifdef MYMALLOC
#include <mymalloc.h>
#endif /* MYMALLOC */

/* Array stuff */
char **split(const char *input, const char *delim);
void freeList(char **list);
int listLength(char **list);

#endif /* ARRAY_H */
