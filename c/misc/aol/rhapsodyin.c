/*
 * Copyright (c) 1998  Software.net
 *
 * $Id: rhapsodyin.c,v 1.1 1998/06/19 04:51:41 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "md5.h"

#define DIGEST_LENGTH 16

struct hmac {
    MD5_CTX md5;
    unsigned int m_key[DIGEST_LENGTH/4];
};

void hmac_init(struct hmac *h, unsigned char *key, int keylength)
{
    static char ipad[]={
        0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36,
        0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36,
        0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36,
        0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36,
    };
    unsigned int inner[4], i;

    MD5Init(&(h->md5));

    if(keylength>DIGEST_LENGTH) {
        printf("Short dog's in the house\n");
        MD5Update(&(h->md5), key, keylength);
        MD5Final(h->m_key, &(h->md5));
        MD5Init(&(h->md5));
    } else {
        bzero( h->m_key, 16);
        memcpy(h->m_key, key, keylength);
    }
    for(i=0; i<4; i++) {
        inner[i]=h->m_key[i]^0x36363636;
    }

    MD5Update(&(h->md5), inner, 16);
    MD5Update(&(h->md5), ipad, sizeof(ipad));
}

void hmac_update(struct hmac *h, unsigned char *input, int length)
{
    MD5Update(&(h->md5), input, length);
}

void hmac_final(struct hmac *h, unsigned char *mac)
{
    unsigned char digest[DIGEST_LENGTH];
    unsigned outer[4];
    static char opad[]={
        0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c,
        0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c,
        0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c,
        0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c, 0x5c,
    };
    int i;

    MD5Final(digest, &(h->md5));
    MD5Init(&(h->md5));

    for(i=0; i<4; i++) {
        outer[i]=h->m_key[i]^0x5c5c5c5c;
    }

    MD5Update(&(h->md5), outer, 16);
    MD5Update(&(h->md5), opad, sizeof(opad));
    MD5Update(&(h->md5), digest, 16);
    MD5Final(mac, &(h->md5));
}

char *hexprint(int size, char *buf)
{
    int i, j=0;
    static char r[1024];
    static char *map="0123456789abcdef";

    for(i=0; i<size; i++) {
        r[j++]=map[((buf[i]&0xf0)>>4)];
        r[j++]=map[(buf[i]&0x0f)];
    }
    r[j]=0x00;
    return(r);
}

#define HMA(a) hmac_update(&h, a, strlen(a))

void main(void)
{
    struct hmac h;
    char mac[16];

    hmac_init(&h, "Jefe", 4);
    hmac_update(&h, "what do ya want for nothing?", 28);
    hmac_final(&h, mac);
    puts(hexprint(16, mac));

    hmac_init(&h, "Jefe", 4);
    HMA("what ");
    HMA("do ");
    HMA("ya ");
    HMA("want ");
    HMA("for ");
    HMA("nothing?");

    hmac_final(&h, mac);
    puts(hexprint(16, mac));
}
