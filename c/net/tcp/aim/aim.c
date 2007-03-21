/*
 * Copyright (c) 1999  Dustin Sallings
 *
 * $Id: aim.c,v 1.2 1999/06/13 07:02:09 dustin Exp $
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

static char *_roasted_string=0x00;

char *aol_roast(char *string)
{
	char *tmp, *roastkey;
	int size=0, i;
	assert(string);

	CONDFREE(roastkey);

	roastkey=_aol_strappend(&size, NULL, AOL_ROAST_STRING);

	while(strlen(roastkey) < strlen(string)) {
		roastkey=_aol_strappend(&size, roastkey, AOL_ROAST_STRING);
	}

	tmp=strdup(string);
	assert(tmp);

	for(i=0; i<strlen(string); i++) {
		tmp[i]=string[i] ^ roastkey[i];
	}

	size=0;
	_roasted_string=_aol_strappend(&size, NULL, "0x");
	_roasted_string=_aol_strappend(&size, _roasted_string,
		_aol_hexprint(strlen(string), tmp));

	free(tmp);
	free(roastkey);

	return(_roasted_string);
}

static void _aol_destroy(AIM *aol)
{
	CONDFREE(_roasted_string);

	if(aol == NULL)
		return;

	CONDFREE(aol->hostname);
	CONDFREE(aol->username);
	CONDFREE(aol->pass);
	CONDFREE(aol->indata.buffer);

	free(aol);
}

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
	assert(aol->hostname);
	aol->username=strdup(username);
	assert(aol->username);
	aol->pass=strdup(aol_roast(pass));
	assert(aol->pass);

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
