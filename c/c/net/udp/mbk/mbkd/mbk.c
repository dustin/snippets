/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: mbk.c,v 1.1 1998/10/03 06:21:11 dustin Exp $
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

struct hashtable *
parsepacket(struct mbk *mbk_packet)
{
	char    buf[MAXPACKETLEN];
	char  **stuff, **kv;
	int     i;

	if (strlen(mbk_packet->data) > MAXPACKETLEN) {
		log_msg("Invalid packet, too long (%d bytes) at %s:%d",
		    strlen(mbk_packet->data), __FILE__, __LINE__);
		return (NULL);
	}
	strcpy(buf, mbk_packet->data);

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

int
verify_auth(char *auth, struct mbk mbk_packet)
{
	MD5_CTX md5;
	char    calc[16], buf[MAXPACKETLEN];
	char   *p;
	struct hash_container *h;

	if (strlen(mbk_packet.data) > MAXPACKETLEN) {
		log_msg("Invalid packet, too long (%d bytes) at %s:%d",
		    strlen(mbk_packet.data), __FILE__, __LINE__);
		return (-1);
	}
	strcpy(buf, mbk_packet.data);
	p = buf;
	p = strrchr(p, ':');
	assert(p);
	*p = 0;

	MD5Init(&md5);
	MD5Update(&md5, auth, strlen(auth));
	MD5Update(&md5, buf, strlen(buf));
	MD5Final(calc, &md5);

	h = hash_find(mbk_packet.hash, "auth");
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

void
sign_data(struct mbk *mbk_packet, char *key)
{
	MD5_CTX md5;
	char    calc[16], buf[16];

	if( strlen(mbk_packet->data) >= (MAXPACKETLEN-40)) {
	    return;
	}

	sprintf(buf, "%d", time(NULL));

	append_data(mbk_packet, "time", buf);

	MD5Init(&md5);
	MD5Update(&md5, AUTHDATA, strlen(AUTHDATA));
	MD5Update(&md5, mbk_packet->data, strlen(mbk_packet->data));
	MD5Final(calc, &md5);

	strcat(mbk_packet->data, ":auth=");
	strcat(mbk_packet->data, hexprint(16, calc));

	mbk_packet->len = 2 * (sizeof mbk_packet->len) + strlen(mbk_packet->data);
}

int
append_data(struct mbk *mbk_packet, char *key, char *value)
{
	int     ret = 0;

	if ((strlen(mbk_packet->data) + strlen(key) +
	     strlen(value)) > (MAXPACKETLEN-64)) {
		ret = -1;
	} else {
		if ((strlen(mbk_packet->data) > 0) &&
		    (mbk_packet->data[strlen(mbk_packet->data) - 1] != ':')) {
			strcat(mbk_packet->data, ":");
		}
		strcat(mbk_packet->data, key);
		strcat(mbk_packet->data, "=");
		strcat(mbk_packet->data, value);
	}

	return (ret);
}

int
send_pkt(struct mbk mbk_packet, char *host, int port, char *auth)
{
    struct hostent *hp;
	struct sockaddr_in sin;
	register int s;

    if ((hp = gethostbyname(host)) == NULL) {
        herror("gethostbyname");
        exit(1);
    }
    if ((s = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        perror("client: socket");
        exit(1);
    }

    sin.sin_family = AF_INET;
    sin.sin_port = htons(port);
    bcopy(hp->h_addr, &sin.sin_addr, hp->h_length);

    sign_data(&mbk_packet, auth);

    if (sendto(s, &mbk_packet, mbk_packet.len,
        0, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
        perror("sendto");
    }
}
