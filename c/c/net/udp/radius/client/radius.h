/*
 * Copyright (c) 1998  SPY Internetworking
 *
 * $Id: radius.h,v 1.1 1998/06/21 08:33:31 dustin Exp $
 */

#define RADIUS_VECTOR_LEN 16
#define RADIUS_PACKET_SIZE 1024
#define RADIUS_PASS_LENGTH 16
#define RADIUS_HEADER_LENGTH (sizeof(unsigned char)*2)+ \
			     sizeof(unsigned short)+ \
			     (sizeof(unsigned char)*RADIUS_VECTOR_LEN)

typedef struct {
    unsigned char attribute;
    unsigned char length;
    unsigned char data[1];
} attribute_t;

typedef struct {
    unsigned char code;
    unsigned char id;
    unsigned short length;
    unsigned char vector[RADIUS_VECTOR_LEN];
    attribute_t att;
} radius_packet;
