/*
 * Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
 *
 * $Id: antinap.h,v 1.1 2001/02/25 06:29:17 dustin Exp $
 */

#ifndef ANTINAP_H
#define ANTINAP_H 1

#define TRAF_UNKNOWN 0
#define TRAF_NAPSTER 1
#define TRAF_GNUTELLA 2

struct antinap {
	int type;
	int fireatwill;
	int mp3count;
};

#endif /* ANTINAP_H */
