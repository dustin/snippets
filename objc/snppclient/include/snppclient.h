/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: snppclient.h,v 1.1 1998/03/17 04:04:57 dustin Exp $
 */

#ifndef _SNPPCLIENT_H_
#define _SNPPCLIENT_H_ 1

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

#endif
