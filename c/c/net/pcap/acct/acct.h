/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: acct.h,v 1.1 2000/07/29 10:33:37 dustin Exp $
 */

#ifndef ACCT_H
#define ACCT_H 1

#define FLAG_BIT(a)	(1<<a)
#define FLAG_PROMISC 0

void process(int flags, char *filter);

#endif /* ACCT_H */
