/*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: main.c,v 1.1 2002/01/21 06:24:30 dustin Exp $
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <assert.h>

#define NOTMATCH 0
#define MATCH 1

/* Global to keep state */
static int lastmatch=NOTMATCH;

/* These are headers we accept */
static char *headers[]={
	/* Basic */
	"From:", "Subject:", "Date:", "Message-Id:", "Sender:"
	/* Mime */
	"MIME-Version:", "Content-Type:", "Content-Transfer-Encoding:",
	/* Extensions */
	"X-",
	NULL
};

/* Send a string */
static void sendString(int fd, const char *str)
{
	int written=0;

	assert(str);

	written=write(fd, str, strlen(str));
	assert(written==strlen(str));
}

/* Check to see if the header is one we want to keep */
static int checkHeader(const char *str)
{
	int i=0;
	int rv=NOTMATCH;

	assert(str);

	if(strlen(str)>0 && isspace(str[0])) {
		rv=lastmatch;
	} else {
		for(i=0; headers[i]!=NULL; i++) {
			if(strncmp(headers[i], str, strlen(headers[i])) == 0) {
				rv=MATCH;
			}
		}
	}

	lastmatch=rv;
	return(rv);
}

/* Remove the whitespace from the end of a string */
static char *kw(char *str)
{
	assert(str);

	if(strlen(str)==0) {
		return(str);
	}

	while(isspace(str[strlen(str)-1])) {
		if(strlen(str)==0) {
			return(str);
		}
		/* Chop it */
		str[strlen(str)-1]=0x00;
	}
	return(str);
}

static void usage(char **argv)
{
	fprintf(stderr, "Usage:  %s newsserver newsgroup\n", argv[0]);
}

int main(int argc, char **argv)
{
	char buf[8192];
	char *newsserver=NULL;
	char *newsgroup=NULL;
	char *tmp;
	int s=1;

	if(argc<3) {
		usage(argv);
		exit(0);
	}

	newsserver=argv[1];
	newsgroup=argv[2];

	/* If I can't complete this post in thirty seconds, I don't want to. */
	alarm(30);

	/* Make the connection */
	s=getclientsocket(newsserver, 119);
	if(s<0) {
		fprintf(stderr, "Couldn't connect.\n");
		exit(1);
	}

	sendString(s, "post\r\n");

	while( (tmp=fgets(buf, sizeof(buf), stdin)) != NULL ) {
		kw(tmp);
		if(strlen(tmp)==0) {
			break;
		}
		if(checkHeader(tmp)==MATCH) {
			/* Send the header and a newline */
			sendString(s, tmp);
			sendString(s, "\r\n");
		}
	}

	/* Done reading headers, add my newsgroup header now */
	sendString(s, "Newsgroups: ");
	sendString(s, newsgroup);
	sendString(s, "\r\n");

	/* Body separator */
	sendString(s, "\r\n");

	/* Body, send this all as-is */
	while( (tmp=fgets(buf, sizeof(buf), stdin)) != NULL ) {
		sendString(s, tmp);
	}

	/* Quit */
	sendString(s, ".\r\nquit\r\n");

	exit(0);
}
