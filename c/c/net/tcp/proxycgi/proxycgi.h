/*
 * Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
 *
 * $Id: proxycgi.h,v 1.1 2000/01/16 04:37:13 dustin Exp $
 */

#ifndef PROXYCGI_H
#define PROXYCGI_H 1

#define ENVCOMPARE(a, b) (getenv(a)!=NULL) && (strcmp(getenv(a), b)==0)
#define GETENV(a) getenv(a)!=NULL?getenv(a):""

struct growstring {
	size_t size;
	char *string;
};

struct url {
	char *host;
	int   port;
	char *req;
	char *httpreq;
};

struct ProxyStruct {
	char *request_uri;
	char *request_url;
	struct url url;
	char *file;
};

void str_append(struct growstring *s, char *buf);
char *kw(char *in);
char **split(char c, char *string);
void freeptrlist(char **list);
int getclientsocket(char *host, int port);

int ensurepath(char *path);

#endif /* PROXYCGI_H */
