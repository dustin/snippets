/*
 * Copyright 1996 SPY Internetworking
 *
 * $Id: parsedetail.c,v 2.7 1997/10/01 07:16:48 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

#include "parse.h"

char **tags;
char **out;
char *timefmt;
char delim;
struct tm ts;

static char *month[]={
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
    "Sep", "Oct", "Nov", "Dec"
};

void dotime(char *t)
{
    struct tm tm;
    t+=4;

    memset(&tm, 0x00, sizeof(struct tm));

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

    ts=tm;
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
    char buf[1024], timebuf[80];
    time_t timestamp;
    struct tm tm;
    int i;

    /* Do something right with the time */
    tm=ts;
    if(timefmt==NULL)
    {
        timestamp=mktime(&tm);
        sprintf(buf, "%d", (int)timestamp);
    }
    else
    {
        strftime(timebuf, 79, timefmt, &tm);
        strcpy(buf, timebuf);
    }

    for(i=0; tags[i]!=NULL; i++)
    {
        buf[strlen(buf)+1]=0x00;
        buf[strlen(buf)]=delim;
        strcat(buf, out[i]);
    }

    puts(buf);
}

void help(char *me)
{
    char str[32];
    char *p;

    /* Pull out version info */
    strcpy(str, "$Revision: 2.7 $");
    for(p=str; *p!=' '; p++);
    p++; p[strlen(p)-1]=0x00;

    printf("%s %s Copyright (c) 1996-1997  Dustin Sallings\n", me, p);
    printf("  Usage: %s [-t timefmt] [-d delim] -l listfile -f infile\n", me);
    puts("       listfile is the list of fields to pull from the RADIUS file");
    puts("       infile is the RADIUS file to read from (- for stdin)");
    puts("       timefmt is a strftime(3) time format");
    puts("       delim is the character to delimit the fields");
}

int main(int argc, char *argv[])
{
char line[80];
FILE *in;
char *listfile=NULL, *infile=NULL;
int i, c;

    timefmt=NULL;
    delim=':';

    while((c=getopt(argc, argv, "l:f:t:d:"))!=-1)
    {
        switch(c)
        {
            case 'l':
                listfile=optarg;
                break;
            case 'f':
                infile=optarg;
                break;
            case 't':
                timefmt=optarg;
                break;
            case 'd':
                delim=optarg[0];
                break;
            case '?':
                help(argv[0]);
                exit(0);
        }
    }

    if(infile==NULL || listfile==NULL)
    {
        help(argv[0]);
        exit(0);
    }

    /* Open the input file */
    if( (strcmp(infile, "-")==0))
    {
        in=stdin;
    }
    else
    {
        in=fopen(infile, "r");
    }
    if(in==NULL)
    {
        perror(infile);
        exit(1);
    }

    /* Get the tags list */
    tags=getList(listfile);

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
