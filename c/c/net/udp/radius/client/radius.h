/*
 * Copyright (c) 1998  SPY Internetworking
 *
 * $Id: radius.h,v 1.3 1998/06/21 21:38:59 dustin Exp $
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

typedef struct {
    char *server;    /* Server hostname */
    int port;        /* server port */
    char *secret;    /* secret for the server */
    int s;           /* server socket */
    radius_packet *rad;
} radius;

/* Yes, I did all this by hand... */

/* Codes */

#define RADIUS_ACCESS_REQUEST         1
#define RADIUS_ACCESS_ACCEPT          2
#define RADIUS_ACCESS_REJECT          3
#define RADIUS_ACCOUNTING_REQUEST     4
#define RADIUS_ACCOUNTING_RESPONSE    5
#define RADIUS_ACCESS_CHALLENGE       11
#define RADIUS_STATUS_SERVER          12
#define RADIUS_STATUS_CLIENT          14

/* things */

#define RADIUS_USERNAME                1
#define RADIUS_PASSWORD                2
#define RADIUS_CHAP_PASSWORD           3
#define RADIUS_CLIENT_ID               4
#define RADIUS_CLIENT_PORT_ID          5
#define RADIUS_USER_SERVICE_TYPE       6
#define RADIUS_FRAMED_PROTOCOL         7
#define RADIUS_FRAMED_ADDRESS          8
#define RADIUS_FRAMED_NETMASK          9
#define RADIUS_FRAMED_ROUTING          10
#define RADIUS_FRAMED_FILTER_ID        11
#define RADIUS_FRAMED_MTU              12
#define RADIUS_FRAMED_COMPRESSION      13
#define RADIUS_LOGIN_HOST              14
#define RADIUS_LOGIN_SERVICE           15
#define RADIUS_LOGIN_TCP_PORT          16
#define RADIUS_OLD_PASSWORD            17
#define RADIUS_PORT_MESSAGE            18
#define RADIUS_DIALBACK_NO             19
#define RADIUS_DIALBACK_NAME           20
#define RADIUS_EXPIRATION              21
#define RADIUS_FRAMED_ROUTE            22
#define RADIUS_FRAMED_IPX_NETWORK      23
#define RADIUS_CHALLENGE_STATE         24
#define RADIUS_ACCT_STATUS_TYPE        40
#define RADIUS_ACCT_DELAY_TIME         41
#define RADIUS_ACCT_SESSION_ID         44
#define RADIUS_ACCT_AUTHENTIC          45
#define RADIUS_ACCT_SESSION_TIME       46

/* User Service Types */

#define RADIUS_LOGIN_USER              1
#define RADIUS_FRAMED_USER             2
#define RADIUS_DIALBACK_LOGIN_USER     3
#define RADIUS_DIALBACK_FRAMED_USER    4
#define RADIUS_OUTBOUND_USER           5
#define RADIUS_SHELL_USER              6
#define RADIUS_AUTH_ONLY               8

/* Framed Protocols */

#define RADIUS_PPP                     1
#define RADIUS_SLIP                    2

/* Framed Routing */

#define RADIUS_NONE                    0
#define RADIUS_BROADCAST               1
#define RADIUS_LISTEN                  2
#define RADIUS_BROADCAST_LISTEN        3

/* Framed compression types */

/* RADIUS_NONE defined above */
#define RADIUS_VAN_JACOBSEN            1

/* Login Services */

#define RADIUS_TELNET                  0
#define RADIUS_RLOGIN                  1
#define RADIUS_TCP_CLEAR               2
#define RADIUS_PORTMASTER              3

/* Status Types */

#define RADIUS_START                   1
#define RADIUS_STOP                    2

/* Auth types */

/* RADIUS_NONE defined above */
#define RADIUS_RADIUS                  1
#define RADIUS_LOCAL                   2

