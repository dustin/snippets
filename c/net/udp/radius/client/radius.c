/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: radius.c,v 1.10 1998/12/26 05:06:00 dustin Exp $
 */

#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <assert.h>

#include "md5.h"
#include "radius.h"
#include "mymalloc.h"

static int getudpsocket(void)
{
    int s;

    if( (s=socket(AF_INET, SOCK_DGRAM, 0)) < 0 ) {
	perror("client: socket");
	return(-1);
    }

    return(s);
}

int rad_send(radius *r)
{
    struct hostent *hp;
    struct sockaddr_in sin;

    if( (hp=gethostbyname(r->server)) == NULL) {
	printf("Couldn't resolve %s\n", r->server);
	return(-1);
    }

    sin.sin_family=AF_INET;
    sin.sin_port=r->port;
    memcpy(&sin.sin_addr, hp->h_addr, hp->h_length);

	r->rad->length=htons(r->rad->length);

    if( sendto(r->s, r->rad, ntohs(r->rad->length), 0,
	(struct sockaddr *)&sin, sizeof(sin)) <0) {
	perror("sendto");
	return(-1);
    }

    return(0);
}

int rad_recv(radius *r)
{
    struct sockaddr sa;
    int salen;
    struct timeval t;
    fd_set fdset;

    salen=sizeof(sa);

    t.tv_sec=3;
    t.tv_usec=0;
    FD_ZERO(&fdset);
    FD_SET(r->s, &fdset);

    if( (select(r->s+1, &fdset, NULL, NULL, &t)) <= 0) {
	return(RADIUS_TIMEOUT);
    }

    recvfrom(r->s, (char *)r->rad, RADIUS_PACKET_SIZE, 0, &sa, &salen);

    /* verify and return */
    return(rad_verify(r));
}

/*
 * This routine was basically stolen from mod_auth_radius
 */

void rad_add_att(radius *r, int type, unsigned char *data, int length)
{
    attribute_t *p;
    radius_packet *rad;

    rad=r->rad;

    p=(attribute_t *) ((unsigned char *)rad + rad->length);
    p->attribute=type;
    p->length=length+2;
    rad->length+=p->length;
    memcpy(p->data, data, length);
}

attribute_t *rad_find_att(radius *r, unsigned char type)
{
    attribute_t *att;
    int len;

    att=&(r->rad->att);
    len=ntohs(r->rad->length)-RADIUS_HEADER_LENGTH;

    while(att->attribute!=type) {
	if( (len-=att->length)<=0) {
	    return(NULL);
	}
	att=(attribute_t *) ((char *)att+att->length);
    }
    return(att);
}

void rad_dump_att(radius *r)
{
    attribute_t *att;
    char printbuf[1024];
    int len, value;

    att=&(r->rad->att);
    len=ntohs(r->rad->length)-RADIUS_HEADER_LENGTH;

    printf("Packet was %d bytes, %d in attributes\n",
           ntohs(r->rad->length), len);

    while(len>0) {
	/* HACK!!! */
	memcpy(&value, att->data, 4);
	printf("Attribute %d, length %d, value: %d\n",
	       att->attribute, att->length,
	       ntohl(value)
	      );
	if(att->length==0) {
		break;
	}
	len-=att->length;
	att=(attribute_t *) ((char *)att+att->length);
    }
}

int rad_verify(radius *r)
{
    MD5_CTX md5;
    unsigned char reply[RADIUS_VECTOR_LEN], calc[RADIUS_VECTOR_LEN];

    memcpy(reply, r->rad->vector, RADIUS_VECTOR_LEN);
    memcpy(r->rad->vector, r->vector, RADIUS_VECTOR_LEN);

    MD5Init(&md5);
    MD5Update(&md5, (unsigned char *)r->rad, ntohs(r->rad->length));
    MD5Update(&md5, r->secret, strlen(r->secret));
    MD5Final(calc, &md5);

    if(memcmp(calc, reply, RADIUS_VECTOR_LEN)!=0)
	return(RADIUS_FAIL_VERIFY);
    return(0);
}

static unsigned char *xor(unsigned char *p, unsigned char *q, int len)
{
    for(;len>=0; len--) *(p++)^=*(q++);
    return(p);
}

int rad_addpass(radius *r, char *password)
{
    MD5_CTX md5, tmpmd5;
    unsigned char misc[1024];
    char pass[RADIUS_PASS_LENGTH];
    int i, len;

    /* Password */

    i=strlen(password);
    len=(i+0x0f)&0xfffffff0; /* round off to 16 */
    if(len==0) {
	len=16;
    } else if(len>128) {
	return(-1);
    }

    memset(pass, 0x00, len);
    memcpy(pass, password, i);

    MD5Init(&md5);
    MD5Update(&md5, r->secret, strlen(r->secret));
    tmpmd5=md5;
    MD5Update(&tmpmd5, r->rad->vector, RADIUS_VECTOR_LEN);
    MD5Final(misc, &tmpmd5);

    xor(pass, misc, RADIUS_PASS_LENGTH);

    for(i=1; i<(len>>4); i++) {
	tmpmd5=md5;
	MD5Update(&tmpmd5, &pass[(i-1)*RADIUS_PASS_LENGTH],RADIUS_PASS_LENGTH);
	MD5Final(misc, &tmpmd5);
	xor(&pass[i*RADIUS_PASS_LENGTH], misc, RADIUS_PASS_LENGTH);
    }

    rad_add_att(r, RADIUS_PASSWORD, pass, 16);

    return(0);
}

int rad_simpleauth(radius *r, char *username, char *password)
{
    MD5_CTX md5;
    int s, i, service, ret;

    r->rad->code=RADIUS_ACCESS_REQUEST;

    /* Username */
    rad_add_att(r, RADIUS_USERNAME, username, strlen(username));
    if(rad_addpass(r, password)<0)
	return(-1);

    r->s=getudpsocket();
    rad_send(r);

    ret=rad_recv(r);

    if(ret>=0)
	ret=r->rad->code;

    return(ret);
}

radius *rad_init(char *hostname, int port, char *secret)
{
    MD5_CTX md5;
    radius *r;
    int i;

    assert(hostname);
    assert(secret);
    assert(port);

    r=calloc(1, sizeof(radius));
    r->rad=calloc(1, RADIUS_PACKET_SIZE);
    r->server=strdup(hostname);
    r->secret=strdup(secret);
    r->port=htons(port);

    srand(time(NULL));
    i=rand()^getpid();
    /* Initialize vectors */
    MD5Init(&md5);
    MD5Update(&md5, &i, 4);
    MD5Final(&(r->rad->vector),&md5);
    memcpy(r->vector, &(r->rad->vector), RADIUS_VECTOR_LEN);

    r->rad->id=getpid()&0xff;
    r->rad->length=RADIUS_HEADER_LENGTH;

    return(r);
}

void rad_destroy(radius *r)
{
    if(r==NULL)
	return;

    if(r->server)
	free(r->server);

    if(r->secret)
	free(r->secret);

    if(r->rad)
	free(r->rad);

    free(r);
}
