/*
 * Check Webserver Status
 * Copyright (c) 1997 SPY Internetworking
 *
 * $Id: getexpire.c,v 1.3 2003/06/04 00:55:30 dustin Exp $
 * $Source: /Users/dustin/stuff/cvstest/c/net/tcp/getexpire.c,v $
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

#include "rsa.h"		/* SSLeay stuff */
#include "crypto.h"
#include "x509.h"
#include "pem.h"
#include "ssl.h"
#include "err.h"

#define CHK_NULL(x) if ((x)==NULL) {printf("Got NULL\n"); exit (1);}
#define CHK_ERR(err,s) if ((err)==-1) { perror(s); exit(1); }
#define CHK_SSL(err) if ((err)==-1) { ERR_print_errors_fp(stderr); exit(2); }

/*
 * It was this or a global variable.  It'll probably change in the future
 */
#define HTMLOUT 1

/*
 * URL request holder.
 */
struct url {
	char   *host;
	int     port;
	char   *req;
};

/*
 * Status return.
 */
struct status {
	int     status;
	int     bytesread;
	char   *message;
};

SSL    *ssl;
SSL_CTX *ctx;

/* Kill Whitey(tm) */
#define iswhitey(a) (a=='\n' || a=='\r')

/*
 * Open and return a tcp socket to host:port, -1 if it fails.
 */

int
openhost(char *host, int port)
{
	struct hostent *hp;
	register int s;
	struct sockaddr_in sin;
	int     err;
	X509   *server_cert;
	char   *str;

	if ((hp = gethostbyname(host)) == NULL) {
		printf("ERR: gethostbyname:  %s\n", host);
		return (-1);
	}
	if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		perror(host);
		return (-1);
	}
	sin.sin_family = AF_INET;
	sin.sin_port = htons(port);
	bcopy(hp->h_addr, &sin.sin_addr, hp->h_length);

	alarm(30);
	if (connect(s, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
		perror(host);
		return (-1);
	}
	alarm(0);

	SSLeay_add_ssl_algorithms();
	SSL_load_error_strings();
	/* ctx = SSL_CTX_new (SSLv23_method());                     CHK_NULL(ctx);
	 */
	ctx = SSL_CTX_new(SSLv2_method());
	CHK_NULL(ctx);

	ssl = SSL_new(ctx);
	CHK_NULL(ssl);
	SSL_set_fd(ssl, s);
	err = SSL_connect(ssl);
	CHK_SSL(err);

	server_cert = SSL_get_peer_certificate(ssl);
	CHK_NULL(server_cert);
	printf("%s:%d:%s\n", host, port,
	    server_cert->cert_info->validity->notAfter->data);

	X509_free(server_cert);

	return (s);
}

void
doit(char *file)
{
	char    buf[1024];
	FILE   *f;
	int     s, i, port;
	char    host[1024];

	f = fopen(file, "r");
	assert(f);

	for (;;) {
		fgets(buf, 1024, f);
		if (feof(f))
			break;

		sscanf(buf, "%s %d", host, &port);

		s = openhost(host, port);
		if (s > 0) {
			close(s);
			SSL_free(ssl);
			SSL_CTX_free(ctx);
		}
	}

	return;
}

int
main(int argc, char **argv)
{
	if (argc < 2) {
		printf("%s, copyright (c) 1997  Dustin Sallings\n"
		    "$Id: getexpire.c,v 1.3 2003/06/04 00:55:30 dustin Exp $\n",
			 argv[0]);
		printf("Error, arguments required.  Usage:\n%s file\n",
		    argv[0]);
		exit(0);
	}
	doit(argv[1]);
}
