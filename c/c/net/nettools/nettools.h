/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: nettools.h,v 1.1 1997/08/09 05:13:53 dustin Exp $
 */

char *nmc_intToDQ(unsigned int addr);
int nmc_addrsToBits(unsigned int addr);
int nmc_bitsToAddrs(int bits);
int nmc_bitsToN(unsigned int bits);
int nmc_countBits(unsigned int addr);
unsigned int nmc_dqToInt(char *dq);
void nmc_testmain(int argc, char **argv);
