/*
 * Copyright (c) 1997 Dustin Sallings
 *
 * $Id: sockets.c,v 1.9 2001/12/07 01:12:49 dustin Exp $
 */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/errno.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <netdb.h>
#include <syslog.h>
#include <netinet/tcp.h>

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif /* HAVE_SYS_TIME_H */

#ifdef USE_SSLEAY
# include <ssl.h>
#endif /* USE_SSLEAY */

#include <splat.h>

extern int _debug;
extern int errno;
extern fd_set rfdset, wfdset;

struct host_ret
getclientsocket(struct url u, int flags)
{
	static struct hostent *hp=NULL;
	int     success, i, flag, port;
	char *host;
	struct host_ret ret;
	struct linger l;
	struct timeval tv;
	struct sockaddr_in sin;
	int fflags=0;
#ifdef USE_SSLEAY
	int     err;
	X509   *server_cert=NULL;
	char   *str=NULL;
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
		if(setsockopt(ret.s, SOL_SOCKET,SO_LINGER,(char *) &l, sizeof(l)) <0) {
			perror("Error setting SO_LINGER");
		}

		tv.tv_sec=0;      /* seconds */
		tv.tv_usec=10000; /* microseconds */
		if(setsockopt(ret.s,SOL_SOCKET,SO_SNDTIMEO,(char *)&tv,sizeof(tv)) <0){
			perror("Error setting send timeout");
		}
		tv.tv_sec=0;      /* seconds */
		tv.tv_usec=10000; /* microseconds */
		if(setsockopt(ret.s,SOL_SOCKET,SO_RCVTIMEO,(char *)&tv,sizeof(tv)) <0){
			perror("Error setting receive timeout");
		}

		/* Set some reasonable (small) timeouts */


		/* Don't disable the nagle algorithm if we say not to */
		if( (flags&DO_NAGLE) == 0) {
			flag = 1;
			if (setsockopt(ret.s, IPPROTO_TCP, TCP_NODELAY, (char *) &flag,
				sizeof(int)) < 0) {
				perror("Unable to disable nagle algorithm");
			}
			flag = 8;
			if (setsockopt(ret.s, SOL_SOCKET, SO_SNDLOWAT, (char *) &flag,
				sizeof(int)) < 0) {
				perror("Unable to set SO_SNDLOWAT");
			}
		}

		/* We're doing non-blocking IO */
		if( (flags&NO_BLOCKING)) {
			fflags = fcntl(ret.s, F_GETFL);
			if(fcntl(ret.s, F_SETFL, fflags | O_NONBLOCK) < 0) {
				perror("fcntl");
			}
		}

		if (connect(ret.s, (struct sockaddr *) &sin, sizeof(sin)) >= 0) {
			success = 1;
			break;
		} else {
			/* If the connect is in progress, it's all good */
			if(errno==EINPROGRESS) {
				success=1;
				break;
			}
		}
	}

	/* If this is succesful, register for both reads and writes */
	if(success) {
		FD_SET(ret.s, &wfdset);
		FD_SET(ret.s, &rfdset);
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

			if (server_cert)
				X509_free(server_cert);
		}
	}
#endif /* USE_SSLEAY */
	return (ret);
}
