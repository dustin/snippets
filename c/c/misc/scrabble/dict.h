/*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: dict.h,v 1.1 2002/02/06 01:47:26 dustin Exp $
 */

#ifndef DICT_H
#define DICT_H 1

#ifndef BOOTSTRAP
#include "dictionary.h"
#endif /* NOT BOOTSTRAP */

struct dict_entry {
	char *word;
	short score;
};

extern struct dict_entry dictionary[];

struct dict_entry *dictDup(struct dict_entry *);

#endif /* DICT_H */
