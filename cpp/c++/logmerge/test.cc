/*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: test.cc,v 1.2 2002/06/01 00:57:02 dustin Exp $
 */

#include "LogFile.h"

int
main(int argc, char **argv)
{
	LogFile lf("testlog");

	cout << "The line:  ";
	cout << lf.getLine() << endl;
	cout << "The line (again):  ";
	cout << lf.getLine() << endl;
	lf.next();
	cout << "The next line:  ";
	cout << lf.getLine() << endl;
}
