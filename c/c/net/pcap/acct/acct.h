/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: acct.h,v 1.2 2000/07/30 03:01:45 dustin Exp $
 */

#ifndef ACCT_H
#define ACCT_H 1

#define FLAG_BIT(a)	(1<<a)
#define FLAG_PROMISC 0

void process(int flags, char *filter);
char *ntoa(int);


#endif /* ACCT_H */
