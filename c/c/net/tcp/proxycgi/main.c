/*
 * Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
 *
 * $Id: main.c,v 1.1 2000/01/16 04:37:13 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <assert.h>

#include "proxycgi.h"

/*
 * Parse a url string into a struct url;
 * port is -1, and host and req are NULL if it fails.
 */
struct url
parseurl(char *url)
{
	char   *tmp;
	struct url u;
	int     port;
	struct growstring grow;

	grow.size = 1024 * sizeof(char);
	grow.string = calloc(1, grow.size);

	u.host = NULL;
	u.req = NULL;
	u.port = -1;

	/* We only do http urls */
	if (strncmp(url, "http://", 7) != 0) {
		return(u);
	}

	/* Host is the first thing, after http:// */
	u.host = strdup(url + 7);

	/*
	 * OK, request comes along eventually, so let's mark it as host, and
	 * go from there.  Look for a :, /, or NULL to let us know we're done
	 * with host.
	 */
	u.req = u.host;
	while (*u.req && *u.req != ':' && *u.req != '/')
		u.req++;

	/*
	 * Let's see which one we got.
	 */
	switch (*u.req) {
	case NULL:		/* format http://host.domain.com */
		u.req = strdup("/");
		port = 80;
		break;
	case ':':		/* format http://host.domain:port/ */
		port = atoi(u.req + 1);
		assert(port);
		*u.req = NULL;
		u.req++;
		while (*u.req && *u.req != '/')
			u.req++;
		u.req = *u.req ? strdup(u.req) : "/";
		break;
	case '/':		/* format http://host.domain.com/ */
		port = 80;
		tmp = u.req;
		u.req = strdup(u.req);
		*tmp = NULL;
		break;
	}

	str_append(&grow, "GET ");
	str_append(&grow, u.req);
	str_append(&grow,  " HTTP/1.0\r\n");
	str_append(&grow,  "Host:  ");
	str_append(&grow,  u.host);
	str_append(&grow,  "\r\n");

	/* This ends the GET request */
	str_append(&grow,  "\r\n");

	u.httpreq=grow.string;

	u.port = port;
	return (u);
}

struct ProxyStruct parseuri(char *uri)
{
	struct ProxyStruct ret;
	char **atmp;
	struct growstring grow;
	int i;

	/* Verify we got something */
	assert(uri);

	/* Initialize the growstring */
	grow.size=1024*sizeof(char);
	grow.string=calloc(sizeof(char), grow.size);

	/* Copy the uri */
	ret.request_uri=strdup(uri);

	/* we prepend each part with a / in the for loop */
	str_append(&grow, "http:/");
	atmp=split('/', uri);
	for(i=5; atmp[i]!=NULL; i++) {
		str_append(&grow, "/");
		str_append(&grow, atmp[i]);
	}

	/* Get the URL stuff */
	ret.request_url=strdup(grow.string);
	ret.url=parseurl(ret.request_url);

	/* Get the file */
	grow.string[0]=0x00;
	str_append(&grow, GETENV("DOCUMENT_ROOT"));
	str_append(&grow, "/");
	str_append(&grow, ret.request_uri);

	ret.file=grow.string;

	freeptrlist(atmp);

	return(ret);
}

int start_request(struct ProxyStruct proxystruct)
{
	int s, size;

	s=getclientsocket(proxystruct.url.host, proxystruct.url.port);
	assert(s>0);

	size=send(s, proxystruct.url.httpreq, strlen(proxystruct.url.httpreq),0);
	assert(size=strlen(proxystruct.url.httpreq));

	return(s);
}

int main(int argc, char **argv)
{
	struct ProxyStruct proxystruct;
	int s, f, size;
	char recv_buf[8192];

	proxystruct=parseuri(GETENV("REQUEST_URI"));

#ifdef UGLY_DEBUG
	printf("URL:  %s\nURI:  %s\nFile:  %s\n",
		proxystruct.request_url, proxystruct.request_uri, proxystruct.file);
	printf("URL Structure:\n\tHost:  %s\n\tPort:  %d\n\tURI:  %s\n",
		proxystruct.url.host, proxystruct.url.port, proxystruct.url.req);
	printf(proxystruct.url.httpreq);
#endif /* UGLY_DEBUG */

	ensurepath(proxystruct.file);
	printf("Creating file:  %s\n", proxystruct.file);
	f=open(proxystruct.file, O_WRONLY|O_CREAT|O_EXCL, 0755);
	if(f<0) {
		perror(proxystruct.file);
	}

	s=start_request(proxystruct);

	while( (size=recv(s, recv_buf, sizeof(recv_buf), 0)) > 0) {
		write(1, recv_buf, size);

		if(f>=0) {
			write(f, recv_buf, size);
		}
	}

	return(0);
}
