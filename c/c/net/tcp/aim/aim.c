/*
 * Copyright (c) 1999  Dustin Sallings
 *
 * $Id: aim.c,v 1.1 1999/06/10 07:30:21 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <syslog.h>
#include <assert.h>

#include "aim.h"

static int _aol_connect(AIM *aol);
static int _aol_logout(AIM *aol);
static void _aol_destroy(AIM *aol);
static char *_aol_read_cr(AIM *aol);

static int _aol_connect(AIM *aol)
{
	assert(aol);

	_ndebug(3, ("Connecting to %s:%d\n", aol->hostname, aol->port));

	if(aol->socket<0) {
		aol->socket=_aol_getclientsocket(aol->hostname, aol->port);
	}

	return(aol->socket);
}

static int _aol_logout(AIM *aol)
{
	assert(aol);

	if(aol->socket<0)
		return(-1);

	close(aol->socket);
	aol->socket=-1;

	return(0);
}

AIM *aol_init(char *hostname, int port, char *username, char *pass)
{
	AIM *aol;
	assert(hostname);
	assert(port > 0 && port < 65535);
	assert(username);
	assert(pass);

	aol=calloc(1, sizeof(AIM));
	assert(aol);
	aol->socket=-1;
	aol->hostname=strdup(hostname);
	aol->username=strdup(username);
	aol->pass=strdup(pass);

	/* Input buffer stuff */
	aol->indata.buf_begin=0;
	aol->indata.buf_end=0;
	aol->indata.buf_size=AOL_BUF_LEN;
	aol->indata.buffer=(char *)malloc(sizeof(char *)*aol->indata.buf_size);
	assert(aol->indata.buffer);
	aol->indata.buffer[0]=0x00;

	aol->connect=_aol_connect;
	aol->logout=_aol_logout;
	aol->destroy=_aol_destroy;
}
