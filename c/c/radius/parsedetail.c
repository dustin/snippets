/*
 * Copyright 1996 SPY Internetworking
 *
 * $Id: parsedetail.c,v 1.1 1997/01/12 09:37:09 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

char *tags[]={
	"User-Name",
	"Acct-Session-Id",
	"Acct-Session-Time",
	"Acct-Status-Type"
};

char *month[]={
	NULL, "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
	"Sep", "Oct", "Nov", "Dec"
};

struct out {
	char userid[20];
	char session_id[20];
	int status_type;
	int session_time;
	int year;
	int day;
	int month;
	int time;
} out;

void dotime(char *t)
{
	t+=4;
	for(out.month=1; out.month<13; out.month++)
		if(strncmp(t, month[out.month], 3)==0) break;

	t+=4;
	out.day=atoi(t);

	t+=3;
	out.time=atoi(t)*3600;

	t+=3;
	out.time+=atoi(t)*60;

	t+=3;
	out.time+=atoi(t);

	t+=3;
	out.year=atoi(t);
}

void insert(char *handle, char *value)
{
register int type;

	for(type=0; type<4; type++)
	{
		if(strcmp(tags[type], handle)==0)
			break;
	}
	switch(type)
	{
		case 0:
			value++; value[strlen(value)-1]=0;
			strcpy(out.userid, value);
			break;
		case 1:
			value++; value[strlen(value)-1]=0;
			strcpy(out.session_id, value);
			break;
		case 2:
			out.session_time=atoi(value);
			break;
		case 3:
			if(strcmp("Start", value))
				out.status_type=0;
			else
				out.status_type=1;
			break;
	}
}

void process(char *line)
{
char *thing[2];
register int i, j=0;

	switch(line[0])
	{
	case 0:
		if(out.status_type==0)
			printf("%s:%s::%d:%d/%d/%d/%d\n", out.userid,
				out.session_id, out.session_time,
				out.month, out.day, out.year, out.time);
		break;
	case '\t':
	/*
		for(i=0; !isalpha(line[i])&&i<strlen(line); i++);
		thing[j++]=line+i;
		for(; line[i]!='=' && i<strlen(line); i++);
		line[i-1]=0;
		thing[j++]=line+i+2;
		insert(thing[0], thing[1]);
	*/
		i=1;
		thing[j++]=line+1;
		for(; line[i]!='=' && i<strlen(line); i++);
		line[i-1]=0;
		thing[j++]=line+i+2;
		insert(thing[0], thing[1]);
		break;
	default:
		dotime(line);
	}
}

int main(int argc, char *argv[])
{
char line[80];
FILE *in;

	if(argc<2)
	{
		fprintf(stderr, "Too few arguments, want filenames\n");
		exit(1);
	}

	if( (in=fopen(argv[1], "r")) == NULL)
	{
		perror(argv[1]);
		exit(2);
	}

	while(fgets(line, 80, in))
	{
		line[strlen(line)-1]=0;
		process(line);
	}

	exit(0);
}
