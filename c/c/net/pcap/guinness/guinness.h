/*
 * Copyright (c) 1998  dustin sallings
 *
 * $Id: guinness.h,v 1.1 1998/10/05 21:45:13 dustin Exp $
 */

#define FLAG_BIT(a)      (1<<a)

#define FLAG_PROMISC     0

void process(int flags);

#ifdef __NetBSD__

int k_getuid(struct in_addr faddr, int fport ,
             struct in_addr laddr, int lport, int *uid);

#endif
