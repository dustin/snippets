/*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: test.cc,v 1.1 2002/05/31 18:25:43 dustin Exp $
 */

#include "LogFile.h"

int
main(int argc, char **argv)
{
	LogFile lf("testlog");

	cout << "The line:  ";
	cout << lf.getLine();
}
