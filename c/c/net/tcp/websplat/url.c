/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: url.c,v 1.2 2000/10/03 10:07:39 dustin Exp $
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <assert.h>

#include <splat.h>

extern int     _debug;

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

	memset(&u, 0x00, sizeof(u));
	u.port = -1;

	/* We only do http and maybe https urls */
	if (strncmp(url, "http://", 7) == 0) {
		u.ssl = 0;
	} else {
#ifdef USE_SSLEAY
		if (strncmp(url, "https://", 7) == 0)
			u.ssl = 1;
		else
#endif
			return (u);
	}

	/* Host is the first thing, after http:// or https:// */
	u.host = strdup(url + 7 + u.ssl);

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
		port = (u.ssl ? 443 : 80);
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
		port = (u.ssl ? 443 : 80);
		tmp = u.req;
		u.req = strdup(u.req);
		*tmp = NULL;
		break;
	}

	if(getenv("WEBSPLAT_POST")) {
		str_append(&grow, "POST ");
	} else {
		str_append(&grow, "GET ");
	}
	str_append(&grow, u.req);
	str_append(&grow,  " HTTP/1.0\r\n");

	str_append(&grow, "Host: ");
	str_append(&grow, u.host);
	str_append(&grow, "\r\n");

	if(getenv("WEBSPLAT_COOKIE")) {
		str_append(&grow,  "Cookie: ");
		str_append(&grow,  getenv("WEBSPLAT_COOKIE"));
		str_append(&grow,  "\r\n");
	}

	if(getenv("WEBSPLAT_POST")) {
		FILE *f;
		char buf[8192];
		struct stat st;
		int ret;

		ret=stat(getenv("WEBSPLAT_POST"), &st);
		assert(ret>=0);

		sprintf(buf, "Content-Length: %d\r\n", (int)st.st_size);
		str_append(&grow, buf);

		str_append(&grow, "Content-Type: application/x-www-form-urlencoded\r\n");

		str_append(&grow,  "\r\n");

		f=fopen(getenv("WEBSPLAT_POST"), "r");
		assert(f);

		while(fgets(buf, 8190, f)) {
			str_append(&grow, buf);
		}

		fclose(f);
	} else {
		/* This ends the request if we're doing a GET */
		str_append(&grow,  "\r\n");
	}

	u.httpreq=grow.string;

	_ndebug(3, ("Request\n%s\n", u.httpreq));

	u.port = port;
	return (u);
}
