/*
 * Copyright 1996 SPY Internetworking
 */

struct cgiform {
    char *name;
    char *value;
    struct cgiform *next;
};

int cgiadd(struct cgiform *d, char *name, char *val);
void cgiprintdata(struct cgiform *d);
char *cgigetdata(struct cgiform *d, char *n);
void cgifree(struct cgiform *d);
struct cgiform *cgiinit(void);
