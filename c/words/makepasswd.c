/*
 * Copyright (c) 1996  Dustin Sallings
 *
 * $Id: makepasswd.c,v 1.2 1997/08/26 06:09:17 dustin Exp $
 */

#include <stdio.h>
#include <time.h>

void makesalt(char *s, int n, long v)
{
    char *c="./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

    while(--n>=0)
    {
	*s++=c[v&0x3f];
	v>>=6;
    }
}

void main(int argc, char **argv)
{
    char salt[3];
    time_t t;

    if(argc>1)
    {
        time(&t);
        srand(t);

        salt[2]=0x00;
        makesalt(salt, 2, rand());

        printf("Encrypted password is %s\n", crypt(argv[1], salt));
    }
}
