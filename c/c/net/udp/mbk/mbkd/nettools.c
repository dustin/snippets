
/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: nettools.c,v 1.3 1998/10/01 18:05:18 dustin Exp $
 */

/*
 * This is Dustin's network number representation library.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <netinet/in.h>

#include <mbkd.h>

int
nmc_bitsToN(unsigned int bits)
{
	unsigned int ret, i;

	ret = 1;

	for (i = 1; i <= bits; i++) {
		ret <<= 1;
		ret++;
	}

	for (i = bits; i <= 31; i++)
		ret <<= 1;

	return (ret);
}

char   *
nmc_intToDQ(unsigned int addr)
{
	unsigned char a[4];
	static char ret[16];

	addr = ntohl(addr);

/*
 * a[0]=a[1]=a[2]=a[3]=addr;
 * a[0]&=0xff000000; a[0]>>=24;
 * a[1]&=0x00ff0000; a[1]>>=16;
 * a[2]&=0x0000ff00; a[2]>>= 8;
 * a[3]&=0x000000ff;
 */

	/* *OR* you could do this: */
	memcpy((void *) a, (void *) &addr, sizeof(int));

	sprintf(ret, "%d.%d.%d.%d", a[0], a[1], a[2], a[3]);

	return (ret);
}

int
nmc_countBits(unsigned int addr)
{
	int     ret = 0;

	if (addr == 0)
		return (0);

	while ((addr & 1) == 0) {
		++ret;
		addr >>= 1;
	}

	return (32 - ret);
}

unsigned int
nmc_dqToInt(char *dq)
{
	char   *a[4];
	int     ret;

	a[0] = strtok(dq, ".");
	if (a[0] == NULL)
		return (0);

	a[1] = strtok(NULL, ".");
	if (a[1] == NULL)
		return (0);

	a[2] = strtok(NULL, ".");
	if (a[2] == NULL)
		return (0);

	a[3] = strtok(NULL, ".");
	if (a[3] == NULL)
		return (0);

	ret = ((atoi(a[0]) << 24) +
	    (atoi(a[1]) << 16) +
	    (atoi(a[2]) << 8) +
	    atoi(a[3]));
	return (ret);
}

int
nmc_addrsToBits(unsigned int addr)
{
	int     i, n;

	n = 1;
	for (i = 0; i < 32; i++) {
		if (n >= addr)
			break;

		n <<= 1;
	}
	return (32 - i);
}

int
nmc_bitsToAddrs(int bits)
{
	int     i, ret;

	if (bits == 0)
		return (0);

	ret = 1;
	for (i = 0; i < (32 - bits); i++)
		ret <<= 1;

	return (ret);
}

#ifdef NEED_MAIN

void
main(int argc, char **argv)
{
	unsigned int x;

	if (argc < 2)
		x = 24;
	else
		x = atoi(argv[1]);

	printf("nmc_bitsToN(%u):  0x%x\n", x, nmc_bitsToN(x));
	printf("nmc_intToDQ(nmc_bitsToN(%u)):  %s\n", x,
	    nmc_intToDQ(nmc_bitsToN(x)));
	printf("nmc_dqToInt(nmc_intToDQ(nmc_bitsToN(%u))):  0x%x\n", x,
	    nmc_dqToInt(nmc_intToDQ(nmc_bitsToN(x))));
	printf("nmc_addrsToBits(12):  %d\n", nmc_addrsToBits(12));
	printf("nmc_countBits(nmc_bitsToN(%u)):  %d\n", x,
	    nmc_countBits(nmc_bitsToN(x)));
	printf("nmc_bitsToAddrs(%u):  %d\n", x, nmc_bitsToAddrs(x));
}
#endif
