/*
 * Copyright 1996 SPY Internetworking
 *
 * $Id: parsedetail.c,v 2.0 1997/09/21 00:28:47 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

#include "parse.h"

char **tags;
char **out;
time_t timestamp;

static char *month[]={
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
    "Sep", "Oct", "Nov", "Dec"
};

void dotime(char *t)
{
    struct tm tm;
    t+=4;

    for(tm.tm_mon=0; tm.tm_mon<12; tm.tm_mon++)
        if(strncmp(t, month[tm.tm_mon], 3)==0) break;

    t+=4;
    tm.tm_mday=atoi(t);

    t+=3;
    tm.tm_hour=atoi(t);

    t+=3;
    tm.tm_min=atoi(t);

    t+=3;
    tm.tm_sec=atoi(t);

    t+=3;
    tm.tm_year=atoi(t)-1900;

    timestamp=mktime(&tm);
}

void insert(char *handle, char *value)
{
register int type;

    for(type=0; tags[type]!=NULL; type++)
    {
        if(strcmp(tags[type], handle)==0)
            break;
    }

    if(tags[type]!=NULL)
    {
	/* Remove quotes */
	if(value[0]=='"')
	{
            value++;
	    value[strlen(value)-1]=0;
	}
        strcpy(out[type], value);
    }
}

void cleanup(void)
{
    int i;
    for(i=0; tags[i]!=NULL; i++)
	out[i][0]=0x00;
}

void process(char *line)
{
char *thing[2];
register int i, j=0;

    switch(line[0])
    {
        case 0:
            display();
	    cleanup();
            break;
        case '\t':
            i=1;
            thing[j++]=line+1;
            for(; line[i]!='=' && i<strlen(line); i++);
            line[i-1]=0;
            thing[j++]=line+i+2;
            insert(thing[0], thing[1]);
            break;
        default:
            dotime(line); }
}

void display(void)
{
    char buf[1024];
    int i;

    sprintf(buf, "%d", timestamp);

    for(i=0; tags[i]!=NULL; i++)
    {
	strcat(buf, ":");
	strcat(buf, out[i]);
    }

    puts(buf);
}

int main(int argc, char *argv[])
{
char line[80];
FILE *in;
int i;

    if(argc<2)
    {
        fprintf(stderr, "Too few arguments, want filenames\n");
        exit(1);
    }

    if( (strcmp(argv[1], "-") == 0) )
    {
	in=stdin;
    }
    else
    {
        if( (in=fopen(argv[1], "r")) == NULL)
        {
            perror(argv[1]);
            exit(2);
        }
    }

    /* Get the tags list */
    tags=getList("list");

    /* Make room for the output */
    for(i=0; tags[i]!=NULL; i++);

    out=(char **)malloc(i*sizeof(char *));

    for(; i>=0; i--)
	out[i]=(char *)malloc(LINELEN*sizeof(char));

    /* process */

    while(fgets(line, 80, in))
    {
        line[strlen(line)-1]=0;
        process(line);
    }

    exit(0);
}
