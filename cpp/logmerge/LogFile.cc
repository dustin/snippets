/*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: LogFile.cc,v 1.1 2002/05/31 18:25:41 dustin Exp $
 */

#include <iostream>
#include <fstream>
#include <string>
#include <assert.h>

#include "LogFile.h"

LogFile::LogFile(string name)
{
	logfilename=name;

	open();
}

void LogFile::open()
{
	const char *c=logfilename.c_str();
	input = new ifstream(c);
	next();
}

void LogFile::next()
{
	getline(*input, line);
}

string LogFile::getLine()
{
	return(line);
}

time_t LogFile::getTimestamp()
{
	return(timestamp);
}
