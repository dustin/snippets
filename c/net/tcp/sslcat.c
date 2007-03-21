/*
 * Check Webserver Status
 * Copyright (c) 1997 SPY Internetworking
 *
 * $Id: sslcat.c,v 1.2 1998/09/02 08:05:24 dustin Exp $
 * $Source: /Users/dustin/stuff/cvstest/c/net/tcp/sslcat.c,v $
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
		printf("ERR: gethostbyname\n");
		return (-1);
	}
	if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		perror("socket");
		return (-1);
	}
	sin.sin_family = AF_INET;
	sin.sin_port = htons(port);
	bcopy(hp->h_addr, &sin.sin_addr, hp->h_length);

	alarm(30);
	if (connect(s, (struct sockaddr *) &sin, sizeof(sin)) < 0)
		return (-1);
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

	printf("SSL connection using %s\n", SSL_get_cipher(ssl));

	server_cert = SSL_get_peer_certificate(ssl);
	CHK_NULL(server_cert);
	printf("Server certificate:\n");

	str = X509_NAME_oneline(X509_get_subject_name(server_cert), NULL, 0);
	CHK_NULL(str);
	printf("\t subject: %s\n", str);
	Free(str);

	str = X509_NAME_oneline(X509_get_issuer_name(server_cert), NULL, 0);
	CHK_NULL(str);
	printf("\t issuer: %s\n", str);
	Free(str);

	X509_free(server_cert);

	return (s);
}

void 
doit(char *host, int port, char *inputfile)
{
	char    buf[1024];
	FILE   *f;
	int     s, i;

	s = openhost(host, port);

	if (s < 0) {
		printf("Couldn't connect.\n");
		return;
	}
	f = fopen(inputfile, "r");
	assert(f);

	for (;;) {
		fgets(buf, 1024, f);
		if (feof(f))
			break;
		SSL_write(ssl, buf, strlen(buf));
	}
	SSL_write(ssl, buf, strlen(buf));

	/* Eat the rest of the page */
	while (i = SSL_read(ssl, buf, 1024)) {
		buf[i] = NULL;
		printf(buf);
		fflush(stdout);
	}

	close(s);
	SSL_free(ssl);
	SSL_CTX_free(ctx);
	return;
}

void 
main(int argc, char **argv)
{
	if (argc < 4) {
		printf("sslcat, copyright (c) 1997  Dustin Sallings\n"
		    "$Id: sslcat.c,v 1.2 1998/09/02 08:05:24 dustin Exp $\n");
		printf("Error, arguments required.  Usage:\n%s host port filename\n",
		    argv[0]);
		exit(0);
	}
	doit(argv[1], atoi(argv[2]), argv[3]);
}
