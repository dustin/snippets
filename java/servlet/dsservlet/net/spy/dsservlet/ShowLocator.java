/*
 * Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
 *
 * $Id: ShowLocator.java,v 1.1 2000/11/07 10:49:03 dustin Exp $
 */

package net.spy.dsservlet;

import java.io.*;
import java.net.*;

/**
 * Show location abstraction.
 */
public class ShowLocator extends Object {

	private String show_id=null;
	private long _length=-1;
	private FileNotFoundException fnfe=null;

	private URL fileUrl=null;
	private File fileObj=null;

	/**
	 * Get a new ShowLocator object representing the passed-in show_id.
	 */
	public ShowLocator(String show_id) {
		super();
		this.show_id=show_id;
	}

	/**
	 * Get the length of the Show this thing represents.
	 */
	public long length() throws IOException {
		if(fnfe!=null) {
			throw fnfe;
		}

		findFile();
		return(_length);
	}

	/**
	 * Get a stream of data from the file.
	 */
	public InputStream getInputStream() throws IOException {
		if(fnfe!=null) {
			throw fnfe;
		}

		InputStream is=null;
		findFile();

		if(fileObj!=null) {
			is=new FileInputStream(fileObj);
		} else {
			is=fileUrl.openStream();
		}
		return(is);
	}

	// Find the file in question
	protected void findFile() throws IOException {
		// Return if we've already got it...
		if(fileObj!=null || fileUrl!=null) {
			return;
		}
		// First, try the local filesystem
		try {
			File f=new File("/afs/spy.net/home/dustin/public_html/ds/"
				+ show_id);
			if(f.isFile()) {
				_length=f.length();
				fileObj=f;
			}
		} catch(Exception e) {
			fileUrl=null;
		}

		// If that didn't work, try the archive
		if(fileObj==null) {
			try {
				fileUrl=new URL( "http://dhcp-121:9999/" + show_id);
				URLConnection uc=fileUrl.openConnection();
				_length=uc.getContentLength();
				InputStream is=uc.getInputStream();
				is.skip(_length);
				is.close();
			} catch(Exception e) {
				fnfe=new FileNotFoundException("Can't find "
					+ show_id + " anywhere!");
				throw fnfe;
			}
		}
	}

	public static void main(String args[]) throws Exception {
		DSBean dsb=new DSBean();
		dsb.setUsername("dustin");

		for(java.util.Enumeration e=dsb.listAll(); e.hasMoreElements(); ) {
			Show show=(Show)e.nextElement();
			ShowLocator sl=show.getLocator();
			long start=System.currentTimeMillis();
			System.out.println("Info for " + show);
			System.out.println("Length:  " + sl.length());
			long end=System.currentTimeMillis();
			System.out.println("Time:  " + (end-start));
			System.out.println("----------------");
		}
	}
}
