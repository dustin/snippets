/*
 * Copyright (c) 1998  dustin sallings
 * portions copyright (c) 1998 beyond.com
 *
 * $Id: http.h,v 1.1 1998/11/11 00:59:53 dustin Exp $
 */

#ifndef HTTP_H
#define HTTP_H 1

#ifdef USE_SSLEAY
#include <rsa.h>
#include <crypto.h>
#include <x509.h>
#include <pem.h>
#include <ssl.h>
#include <err.h>

#define CHK_NULL(x) if ((x)==NULL) {printf("Got NULL\n"); exit (1);}
#define CHK_ERR(err,s) if ((err)==-1) { perror(s); exit(1); }
#define CHK_SSL(err) if ((err)==-1) { ERR_print_errors_fp(stderr); exit(2); }

#endif /* USE_SSLEAY */

/* Kill Whitey(tm) */
#define iswhitey(a) (a=='\n' || a=='\r')

#ifndef HAVE_VSNPRINTF
#define vsnprintf(a, b, c, d) vsprintf(a, c, d)
#endif

/*
 * URL request holder.
 */
struct url {
    char   *host;
    int     port;
    char   *req;
    int     ssl;
};

/*
 * Status return.
 */
struct status {
    int     status;
    int     bytesread;
    char   *message;
};

/*
 * Host return
 */

struct host_ret {
    int     s;
#ifdef USE_SSLEAY
    SSL    *ssl;
    SSL_CTX *ctx;
#endif
};

int recv_data(struct host_ret conn, struct url u, char *buf, size_t len);
int send_data(struct host_ret conn, struct url u, char *data);
int send_ndata(struct host_ret conn, struct url u, size_t n, char *data);
struct host_ret openhost(char *host, int port, int dossl);
struct status getstatus(struct url u, struct host_ret conn);
struct url parseurl(char *url);
void freestatus(struct status s);
void freeurl(struct url u);
void timeout(int c);


#endif /* HTTP_H */
