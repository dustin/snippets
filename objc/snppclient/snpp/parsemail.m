/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: parsemail.m,v 1.1 1998/04/17 05:31:47 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>

#include <dSNPP.h>

extern char *optarg;
extern int optind, opterr;

#define LINELEN 2048

#define PR_HIGH   "high"
#define PR_NORMAL "normal"

static char *ckw(char *in)
{
    /* bounds checking */
    if(strlen(in)==0)
        return(in);

    while(isspace(in[strlen(in)-1]))
    {
        /* bounds checking */
        if(strlen(in)==0)
            return(in);

        in[strlen(in)-1]=0x00;
    }

    /* Returns its arg */
    return(in);
}

static void usage(char *name)
{
    fprintf(stderr, "Usage:\n%s [-p priority] [-t tag] <to>\n", name);
}

static char *getdata(int l, char *line)
{
    char *ret;

    ret=(char *)malloc(strlen(line));
    strcpy(ret, line+l);
    ckw(ret);
    return(ret);
}


int main(int argc, char **argv)
{
    char line[LINELEN];
    int c, r=0;
    char *priority;
    char *subject="(no subject)", *from=NULL, *to=NULL, *tag="Mail";
    extern int optind;
    id snpp;

    snpp=[[dSNPP alloc] init];

    priority=PR_NORMAL;

    while( (c=getopt(argc, argv, "p:t:")) != -1)
    {
	switch(c)
	{
	    case 'p':
		if(tolower(optarg[0])=='h')
		    priority=PR_HIGH; break;
            case 't':
                tag=optarg; break;
	    case '?':
		usage(argv[0]); exit(1); break;
	}
    }

    if(optind>=argc)
    {
	fputs("Error, too few arguments\n", stderr);
	usage(argv[0]);
	exit(1);
    }

    to=argv[optind++];

    while(!feof(stdin))
    {
	if(fgets(line, LINELEN, stdin)==NULL)
            break;

	if(strncmp(line, "From: ", 6) == 0)
	{
	    from=getdata(6, line);
	}
	else if(strncmp(line, "Subject: ", 9) == 0)
	{
	    subject=getdata(9, line);
	}
    }

    sprintf(line, "%s: %s -- %s", tag, from, subject);

    if([snpp connectTo :"pager" :1041]<0) {
	r=75;
    } else {
        if([snpp sendAPage :to thatsays:line priority:priority]<0)
	    r=75;
        [snpp quit];
    }

    free(from);
    free(subject);
    return(r);
}
