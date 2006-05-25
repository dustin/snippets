/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: acct.h,v 1.3 2000/07/30 04:58:00 dustin Exp $
 */

#ifndef MULTISNIFF_H
#define MULTISNIFF_H 1

#define FLAG_BIT(a)	(1<<a)
#define FLAG_PROMISC 0

#define PTHREAD_PRINT_INTERVAL 5
#define NON_PTHREAD_PRINT_INTERVAL 5

#define FILENAME_MAXLEN 64

/* Maximum number of seconds we'll hold a pcap file open. */
#define MAX_PKT_AGE 60

void process(int flags, const char *intf, const char *outdir, char *filter);
char *ntoa(int);

#endif /* MULTISNIFF_H */
