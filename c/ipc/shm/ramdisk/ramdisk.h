/*
 * Copyright (c) 1997  SPY Internetworking
 *
 * $Id: ramdisk.h,v 1.1 1997/08/26 06:00:10 dustin Exp $
 */

#ifndef _RAMDISK_H
#define _RAMDISK_H 1

#define SHM_SIZE 4000000
#define SHM_KEY  2600

struct head {
    int shm_size;
    int maxent;
};

struct tocent {
    int size;
    int perm;
    int offset;
};

#endif
