/*
 * Check Webserver Status
 * Copyright (c) 1997 SPY Internetworking
 *
 * $Id: http.c,v 1.5 1998/11/11 07:16:15 dustin Exp $
 * $Source: /Users/dustin/stuff/cvstest/c/net/tcp/hcp/http.c,v $
 *
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <assert.h>
#include <ctype.h>

#ifndef HAVE_SNPRINTF
#include <stdarg.h>
#endif /* HAVE_SNPRINTF */

#include "http.h"

#ifndef HAVE_SNPRINTF
/*
 * snprintf for those that don't have it.
 * More that likely, it'll overrun buffers, because they
 * probably don't have vsnprintf either.
 */
int snprintf(char *s, size_t n, const char *format, ...)
{
	int i;
	va_list ap;
	va_start(ap, format);
	i=vsnprintf(s, n-1, format, ap);
	va_end(ap);
	return(i);
}
#endif

/*
 * Open and return a tcp socket to host:port, -1 if it fails.
 */

struct host_ret
openhost(char *host, int port, int dossl)
{
	struct hostent *hp;
	struct sockaddr_in sin;
	struct host_ret ret;
#ifdef USE_SSLEAY
	int     err;
	X509   *server_cert;
	char   *str;
#endif

	ret.s = -1;

	if ((hp = gethostbyname(host)) == NULL) {
		fprintf(stderr, "ERR: gethostbyname: %s\n", host);
		return (ret);
	}
	if ((ret.s = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		perror("socket");
		return (ret);
	}
	sin.sin_family = AF_INET;
	sin.sin_port = htons(port);
	bcopy(hp->h_addr, &sin.sin_addr, hp->h_length);

	alarm(30);
	if (connect(ret.s, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
		close(ret.s);
		ret.s = -1;
		return (ret);
	}
	alarm(0);

#ifdef USE_SSLEAY
	if (dossl) {
		SSLeay_add_ssl_algorithms();
		SSL_load_error_strings();
		ret.ctx = SSL_CTX_new(SSLv2_method());
		CHK_NULL(ret.ctx);

		ret.ssl = SSL_new(ret.ctx);
		CHK_NULL(ret.ssl);
		SSL_set_fd(ret.ssl, ret.s);
		err = SSL_connect(ret.ssl);
		CHK_SSL(err);

		server_cert = SSL_get_peer_certificate(ret.ssl);
		CHK_NULL(server_cert);

		str = X509_NAME_oneline(X509_get_subject_name(server_cert), NULL, 0);
		CHK_NULL(str);
		Free(str);

		str = X509_NAME_oneline(X509_get_issuer_name(server_cert), NULL, 0);
		CHK_NULL(str);
		Free(str);

		if (server_cert)
			X509_free(server_cert);
	}
#endif /* USE_SSLEAY */

	return (ret);
}

/* What to do when a connection times out.  */
RETSIGTYPE
timeout(int c)
{
	alarm(0);
	signal(SIGALRM, timeout);
	return;
}

/*
 * Parse a url string into a struct url;
 * port is -1, and host and req are NULL if it fails.
 */
struct url
parseurl(char *url)
{
	char   *tmp;
	struct url u;
	int     i, port;

	u.host = NULL;
	u.req = NULL;
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

	u.port = port;
	return (u);
}

/*
 * Free any data that might be in a struct url
 */
void
freeurl(struct url u)
{
	if (u.host)
		free(u.host);
	if (u.req)
		free(u.req);
}

/*
 * Free any data that might be in a struct status
 */
void
freestatus(struct status s)
{
	if (s.message)
		free(s.message);
}

int
send_data(struct host_ret conn, struct url u, char *data)
{
	if (u.ssl) {
#ifdef USE_SSLEAY
		SSL_write(conn.ssl, data, strlen(data));
#endif
	} else {
		send(conn.s, data, strlen(data), 0);
	}
}

int
send_ndata(struct host_ret conn, struct url u, size_t n, char *data)
{
	if (u.ssl) {
#ifdef USE_SSLEAY
		SSL_write(conn.ssl, data, n);
#endif
	} else {
		send(conn.s, data, n, 0);
	}
}

int
recv_data(struct host_ret conn, struct url u, char *buf, size_t len)
{
	int     size;
	if (u.ssl) {
#ifdef USE_SSLEAY
		size = SSL_read(conn.ssl, buf, len);
#else
		assert(u.ssl == 0);
#endif
	} else {
		size = recv(conn.s, buf, len, 0);
	}
	return (size);
}

/*
 * Accept a url string, return a struct status.
 *
 * A negative status indicates a problem either connecting to the
 * machine, or a url parse problem.  The message will tell you what,
 * specifically happened (although it doesn't distinguish between a
 * timeout, and a connection refused).
 */
struct status
getstatus(struct url u, struct host_ret conn)
{
	int     i;
	char    line[1024];
	char   *p, *q;
	struct status st;

	st.status = -1;
	st.message = NULL;
	st.bytesread = 0;

	alarm(120);
	i = recv_data(conn, u, line, 1024);
	alarm(0);
	if (i < 1) {
		st.message = strdup("Timeout, or nothing returned.");
		return (st);
	}
	line[i] = NULL;

	/*
	 * My keen parsing techniques, flip through it with a pointer
	 * to get the status number
	 */
	p = &line[0];
	while (*p++ && *p != ' ');
	st.status = atoi(p);

	/* Now we want the status message */
	while (*++p && *p != ' ');

	/* Kill Whitey */
	q = p;
	while (*++q && !iswhitey(*q));
	*q = NULL;
	st.message = strdup(p + 1);

	/* Eat the rest of the page */
	while (recv_data(conn, u, line, 1024));

	return(st);
}

/*
 * Print a status struct
 */
void
printstatus(char *url, struct status st)
{
#if HTMLOUT
	printf("\t<li><font color=\"#%s\">%d</font> -- "
	    "<a href=\"%s\">%s</a></li>\n",
	    st.status == 200 ? "007f00" : "ff0000",
	    st.status,
	    url, url);
#else
	printf("%s:  %d (%s)\n", url, st.status, st.message);
#endif
}
