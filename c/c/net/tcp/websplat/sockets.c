/*
 * Copyright (c) 1997 Dustin Sallings
 *
 * $Id: sockets.c,v 1.5 1999/06/16 06:33:29 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <syslog.h>
#include <netinet/tcp.h>

#ifdef USE_SSLEAY
# include <ssl.h>
#endif /* USE_SSLEAY */

#include <splat.h>

extern int _debug;

struct host_ret
getclientsocket(struct url u, int flags)
{
	static struct hostent *hp=NULL;
	int     success, i, flag, port;
	char *host;
	struct host_ret ret;
	struct linger l;
	struct sockaddr_in sin;
#ifdef USE_SSLEAY
	int     err;
	X509   *server_cert;
	char   *str;
#endif /* USE_SSLEAY */

	host=u.host;
	port=u.port;

	/* Zero it out */
	memset(&ret, 0x00, sizeof(ret));

	ret.s=-1;

	_ndebug(2, ("Building client socket for %s:%d\n", host, port));

	if (host == NULL || port == 0)
		return (ret);

	if(hp==NULL) {
		if ((hp = gethostbyname(host)) == NULL) {
#ifdef HAVE_HERROR
			herror("gethostbyname");
#else
			fprintf(stderr, "Error looking up %s\n", host);
#endif
			return (ret);
		}
	}
	success = 0;

	/* of course, replace that 1 with the max number of con attempts */
	for (i = 0; i < 1; i++) {
		if ((ret.s = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
			perror("socket");
			return (ret);
		}
		sin.sin_family = AF_INET;
		sin.sin_port = htons(port);
		memcpy(&sin.sin_addr, hp->h_addr, hp->h_length);

		l.l_onoff = 1;
		l.l_linger = 60;
		setsockopt(ret.s, SOL_SOCKET, SO_LINGER, (char *) &l, sizeof(l));

		/* Don't disable the nagle algorithm if we say not to */
		if( (flags&DO_NAGLE) ==0) {
			flag = 1;
			if (setsockopt(ret.s, IPPROTO_TCP, TCP_NODELAY, (char *) &flag,
				sizeof(int)) < 0) {
				puts("Nagle algorithm not dislabled.");
			}
		}

		if (connect(ret.s, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
			sleep(1);
		} else {
			success = 1;
			break;
		}
	}

	if (!success)
		ret.s = -1;

#ifdef USE_SSLEAY
	if(success && u.ssl) {
#if(SSLEAY_VERSION_NUMBER>=0x0800)
		SSLeay_add_ssl_algorithms();
#endif /* SSLEAY_VERSION_NUMBER>=0x0800 */
		SSL_load_error_strings();
#if(SSLEAY_VERSION_NUMBER<0x0800)
		ret.ctx = SSL_CTX_new();
#else
		ret.ctx = SSL_CTX_new(SSLv2_method());
#endif /* SSLEAY_VERSION_NUMBER<0x0800 */

		CHK_NULL(ret.ctx);

		ret.ssl = SSL_new(ret.ctx);
		CHK_NULL(ret.ssl);

		SSL_set_fd(ret.ssl, ret.s);
		err = SSL_connect(ret.ssl);

		_ndebug(2, ("Checking SSL\n"));
		CHK_SSL(err);
		_ndebug(2, ("Done checking SSL\n"));

		if(_debug>0) {

			server_cert = SSL_get_peer_certificate(ret.ssl);
			CHK_NULL(server_cert);

#if(SSLEAY_VERSION_NUMBER<0x0800)
			str = X509_NAME_oneline(X509_get_subject_name(server_cert));
#else
			str = X509_NAME_oneline(X509_get_subject_name(server_cert),NULL,0);
#endif

			printf("Cert:  %s\n", str);

			CHK_NULL(str);
			Free(str);

#if(SSLEAY_VERSION_NUMBER<0x0800)
			str = X509_NAME_oneline(X509_get_issuer_name(server_cert));
#else
			str = X509_NAME_oneline(X509_get_issuer_name(server_cert),NULL,0);
#endif

			printf("Issuer:  %s\n", str);

			CHK_NULL(str);
			Free(str);
		}

		if (server_cert)
			X509_free(server_cert);
	}
#endif /* USE_SSLEAY */
	return (ret);
}
