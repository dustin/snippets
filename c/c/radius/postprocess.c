/*
 * Copyright 1996 SPY Internetworking
 *
 * $Id: postprocess.c,v 2.0 1997/09/21 00:28:48 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <ndbm.h>

DBM *db;

void store(char *what, int value)
{
datum name, val;
char blah[15];
int old;


    name.dptr=what;
    name.dsize=strlen(what);

    val=dbm_fetch(db, name);
    if(val.dptr == NULL)
        old=0;
    else
        old=atoi(val.dptr);

    sprintf(blah, "%d", value+old);

    val.dptr=blah;
    val.dsize=strlen(blah);

    dbm_store(db, name, val, DBM_REPLACE);
}

void dothe(char *line)
{
char line2[80];
int i, j;

    for(i=0; line[i]!=':'; i++)
    {
        line2[i]=line[i];
    }
    line2[i++]=0;

    for(j=0; j<2; j++)
    {
        for(;+line[i]!=':'; i++);
        i++;
    }

    store(line2, atoi(line+i));
}

void main(int argc, char *argv[])
{
FILE *f;
char line[80];

    db=dbm_open("usage", O_RDWR | O_CREAT, 0644);

    if( (strcmp(argv[1], "-")==0))
    {
        f=stdin;
    }
    else
    {
        f=fopen(argv[1], "r");
    }

    while(fgets(line, 80, f))
    {
        dothe(line);
    }

    dbm_close(db);
}
