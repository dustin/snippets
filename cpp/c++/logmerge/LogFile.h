/*
 * Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
 *
 * $Id: LogFile.h,v 1.1 2002/05/31 18:25:42 dustin Exp $
 */

#ifndef LOGFILE_H
#define LOGFILE_H 1

#include <string>
#include <iostream>

//! This class is used to represent logfiles that will be read.
class LogFile {
	public:
		//! Get a LogFile for the given LogFile
		LogFile(string logfilename);
		//! Get the current line of the file.
		string getLine();
		//! Get the timestamp of the current line.
		time_t getTimestamp();
		//! Seek to the next line in the file.
		void next();

	private:
		//! Current line's timestamp
		time_t timestamp;
		//! Current line
		string line;
		//! The input stream that's being read.
		istream *input;
		//! The name of the logfile.
		string logfilename;

		//! Open the log file
		void open();
};

#endif /* LOGFILE_H */
