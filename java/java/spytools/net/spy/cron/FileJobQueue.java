// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: FileJobQueue.java,v 1.1 2001/04/03 07:37:23 dustin Exp $

package net.spy.cron;

import java.io.*;
import java.util.*;
import java.text.*;

import net.spy.SpyUtil;

/**
 * Get a job queue as defined in a file.  The file will be in the following
 * format:
 *
 * <p>
 *
 * <pre>
 * YYYYMMDD-HHMMSS calfield calincrement classname args ...
 * </pre>
 *
 * <p>
 *
 * For example:
 *
 * <pre>
 * 20010403-090000 DAY 1 net.spy.pagermusic.RunSubs
 * </pre>
 */
public class FileJobQueue extends JobQueue {

	private File sourceFile=null;

	/**
	 * Get a new FileJobQueue based on what's in the given file.
	 */
	public FileJobQueue(File f) throws IOException {
		super();
		this.sourceFile=f;

		initQueue();
	}

	// Init the job queue.
	private void initQueue() throws IOException {
		FileReader fr=new FileReader(sourceFile);
		LineNumberReader lnr=new LineNumberReader(fr);

		String line=lnr.readLine();
		while(line!=null) {

			try {
				Job j=parseJob(line);
				if(j!=null) {
					addJob(j);
				}
			} catch(Exception e) {
				System.err.println("Error parsing line "
					+ lnr.getLineNumber() + ":  " + e);
			}

			line=lnr.readLine();
		}

		lnr.close();
	}

	// Parse an individual line from the job file.
	private Job parseJob(String line) throws ParseException {
		Job rv=null;

		line=line.trim();

		// Ignore comments.
		if(line.startsWith("#")) {
			return(null);
		}

		// Ignore empty lines.
		if(line.length() < 1) {
			return(null);
		}

		String stuff[]=SpyUtil.split(" ", line);
		String date_s=stuff[0];
		String field_s=stuff[1];
		String incr_s=stuff[2];
		String class_s=stuff[3];

		String args[]=new String[stuff.length-4];
		// If there were args, copy them in instead.
		if(stuff.length>4) {
			System.arraycopy(stuff, 4, args, 0, args.length);
		}

		SimpleDateFormat df=new SimpleDateFormat("yyyyMMdd-hhmmss");
		Date startDate=df.parse(date_s);

		int cf=parseCalendarField(field_s);
		if(cf>=0) {
			TimeIncrement ti=new TimeIncrement();
			ti.setField(cf);
			ti.setIncrement(Integer.parseInt(incr_s));

			rv=new MainJob(class_s, args, startDate, ti);
		} else {
			rv=new MainJob(class_s, args, startDate);
		}

		return(rv);
	}

	private int parseCalendarField(String field_name) {
		int rv=-1;

		if(field_name.equals("YEAR")) {
			rv=Calendar.YEAR;
		} else if(field_name.equals("MONTH")) {
			rv=Calendar.MONTH;
		} else if(field_name.equals("DAY")) {
			rv=Calendar.DAY_OF_MONTH;
		} else if(field_name.equals("HOUR")) {
			rv=Calendar.HOUR;
		} else if(field_name.equals("MINUTE")) {
			rv=Calendar.MINUTE;
		} else if(field_name.equals("SECOND")) {
			rv=Calendar.SECOND;
		} else {
			System.err.println("WARNING!  " + field_name
				+ " is not a valid Calendar field.");
		}

		return(rv);
	}

}
