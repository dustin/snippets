/*
 * Copyright (c) 1998-1999  Dustin Sallings
 *
 * $Id: utility.c,v 1.1 1999/06/10 07:30:23 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <assert.h>

#include "aim.h"

#ifndef HAVE_SNPRINTF
int snprintf(char *s, size_t n, const char *format, ...)
{
    va_list ap;
    int r;

    va_start(ap, format);
    r=vsnprintf(s, n, format, ap);
    va_end(ap);

    return(r);
}
#endif

/* binary -> ascii */

char *_aol_hexprint(int size, char *buf)
{
    int i, j=0;
    static char r[1024];
    static char *map="0123456789abcdef";

    for(i=0; i<size; i++) {
        r[j++]=map[((buf[i]&0xf0)>>4)];
        r[j++]=map[(buf[i]&0x0f)];
    }
    r[j]=0x00;
    return(r);
}

/* ascii -> binary */

char *_aol_unhexprint(int size, char *buf)
{
    int i, j=0;
    static char r[1024];
    int map[256];

    for(i=0; i<256; i++) {
        if(i>='0' && i<='9') {
            map[i]=i-'0';
        } else if(i>='a' && i<='f') {
            map[i]= (i-'a')+10;
        } else if(i>='A' && i<='F') {
            map[i]= (i-'A')+10;
        } else {
            map[i]=-1;
        }
    }

    for(i=0; i<size*2; i+=2) {
        assert(map[buf[i]]>=0 && map[buf[i+1]]>=0);
        r[j++]=(map[buf[i]]<<4 | map[buf[i+1]]);
    }

    return(r);
}

char *_aol_kw(char *command)
{
    static char cmdbuf[1024];
    char *c;
    int j=0;

    for(c=command; *c; c++) {
        if(!isspace(*c)) {
            cmdbuf[j++]=*c;
        }
    }
    cmdbuf[j]=0x00;

    return(cmdbuf);
}

char *_aol_killwhitey(char *in)
{
    /* bounds checking */
    if(strlen(in)==0)
        return(in);

    while(isspace(in[strlen(in)-1])) {
        /* bounds checking */
        if(strlen(in)==0)
            return(in);

        in[strlen(in)-1]=0x00;
    }

    return(in);
}
