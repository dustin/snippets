/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: mbk.c,v 1.2 1998/10/03 07:29:16 dustin Exp $
 */

#include <config.h>
#include <mbkd.h>
#include <readconfig.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <assert.h>

#include <md5.h>

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#define AUTHDATA "630712e3e78e9ac261f13b8918c1dbdc"

static struct hashtable *
_mbk_parsepacket(MBK *mbk_packet)
{
	char    buf[MAXPACKETLEN];
	char  **stuff, **kv;
	int     i;

	if (strlen(mbk_packet->pkt.data) > MAXPACKETLEN) {
		log_msg("Invalid packet, too long (%d bytes) at %s:%d",
		    strlen(mbk_packet->pkt.data), __FILE__, __LINE__);
		return (NULL);
	}
	strcpy(buf, mbk_packet->pkt.data);

	mbk_packet->hash = hash_init(HASHSIZE);
	if (mbk_packet->hash == NULL) {
		return (NULL);
	}
	stuff = split(':', buf);
	for (i = 0; stuff[i]; i++) {
		kv = split('=', stuff[i]);
		hash_store(mbk_packet->hash, kv[0], kv[1]);
		freeptrlist(kv);
	}

	freeptrlist(stuff);

	return (mbk_packet->hash);
}

static int
_mbk_verify_auth(MBK *mbk_packet)
{
	MD5_CTX md5;
	char    calc[16], buf[MAXPACKETLEN];
	char   *p;
	struct hash_container *h;

	if (strlen(mbk_packet->pkt.data) > MAXPACKETLEN) {
		log_msg("Invalid packet, too long (%d bytes) at %s:%d",
		    strlen(mbk_packet->pkt.data), __FILE__, __LINE__);
		return (-1);
	}
	strcpy(buf, mbk_packet->pkt.data);
	p = buf;
	p = strrchr(p, ':');
	assert(p);
	*p = 0;

	MD5Init(&md5);
	MD5Update(&md5, mbk_packet->auth, strlen(mbk_packet->auth));
	MD5Update(&md5, buf, strlen(buf));
	MD5Final(calc, &md5);

	h = hash_find(mbk_packet->hash, "auth");
	if (h == NULL) {
		log_msg("Invalid packet, received no auth at %s:%d",
		    __FILE__, __LINE__);
		return (-1);
	}
	if (strcmp(hexprint(16, calc), h->value) == 0) {
		log_msg("Verified auth, packet is good.");
		return (0);
	} else {
		log_msg("Invalid packet, ignoring at %s:%d.", __FILE__, __LINE__);
		return (-1);
	}
}

static void
_mbk_sign_data(MBK *mbk_packet)
{
	MD5_CTX md5;
	char    calc[16], buf[16];

	if( strlen(mbk_packet->pkt.data) >= (MAXPACKETLEN-40)) {
	    return;
	}

	sprintf(buf, "%d", time(NULL));

	mbk_packet->append(mbk_packet, "time", buf);

	MD5Init(&md5);
	MD5Update(&md5, AUTHDATA, strlen(AUTHDATA));
	MD5Update(&md5, mbk_packet->pkt.data, strlen(mbk_packet->pkt.data));
	MD5Final(calc, &md5);

	strcat(mbk_packet->pkt.data, ":auth=");
	strcat(mbk_packet->pkt.data, hexprint(16, calc));

	mbk_packet->pkt.len = 2 * (sizeof mbk_packet->pkt.len) +
	                      strlen(mbk_packet->pkt.data);
}

static int
_mbk_append_data(MBK *mbk_packet, char *key, char *value)
{
	int     ret = 0;

	if ((strlen(mbk_packet->pkt.data) + strlen(key) +
	     strlen(value)) > (MAXPACKETLEN-64)) {
		ret = -1;
	} else {
		if ((strlen(mbk_packet->pkt.data) > 0) &&
		    (mbk_packet->pkt.data[strlen(mbk_packet->pkt.data) - 1] != ':')) {
			strcat(mbk_packet->pkt.data, ":");
		}
		strcat(mbk_packet->pkt.data, key);
		strcat(mbk_packet->pkt.data, "=");
		strcat(mbk_packet->pkt.data, value);
	}

	return (ret);
}

static int
_mbk_send_pkt(MBK *mbk_packet)
{
    struct hostent *hp;
	struct sockaddr_in sin;
	register int s;

    if ((hp = gethostbyname(mbk_packet->host)) == NULL) {
        herror("gethostbyname");
        exit(1);
    }
    if ((s = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        perror("client: socket");
        exit(1);
    }

    sin.sin_family = AF_INET;
    sin.sin_port = htons(mbk_packet->port);
    bcopy(hp->h_addr, &sin.sin_addr, hp->h_length);

    mbk_packet->sign(mbk_packet);

    if (sendto(s, &(mbk_packet->pkt), mbk_packet->pkt.len,
        0, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
        perror("sendto");
    }
}

static void
_mbk_destroy(MBK *mbk)
{
    if(!mbk)
	    return;

    if(mbk->host)
	    free(mbk->host);
    if(mbk->auth)
	    free(mbk->auth);
    if(mbk->hash)
	    hash_destroy(mbk->hash);

    free(mbk);
}

MBK *mbk_new(char *host, int port, char *auth)
{
    MBK *mbk_packet;

	mbk_packet=calloc(sizeof(MBK), 1);
	assert(mbk_packet);

	if(host)
	    mbk_packet->host=strdup(host);

	mbk_packet->port=port;

	if(auth)
	    mbk_packet->auth=strdup(auth);

	mbk_packet->append=_mbk_append_data;
	mbk_packet->destroy=_mbk_destroy;
	mbk_packet->parse=_mbk_parsepacket;
	mbk_packet->send=_mbk_send_pkt;
	mbk_packet->sign=_mbk_sign_data;
	mbk_packet->verify=_mbk_verify_auth;

	return(mbk_packet);
}
