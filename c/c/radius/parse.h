/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: parse.h,v 2.1 1997/09/21 00:30:19 dustin Exp $
 */

#define LINELEN 1024

char **getList(char *filename);
char *kw(char *in);
void cleanup(void);
void display(void);
void freeList(char **list);
