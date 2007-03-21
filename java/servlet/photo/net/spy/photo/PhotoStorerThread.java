/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoStorerThread.java,v 1.8 2000/03/17 09:41:31 dustin Exp $
 */

package net.spy.photo;

import java.sql.*;
import java.lang.*;
import java.util.*;
import java.io.*;
import sun.misc.*;

import net.spy.*;

public class PhotoStorerThread extends Thread {

	// Constructor
	public PhotoStorerThread() {
		super("storer_thread");
		this.setDaemon(true);
	}

	// Query to store an image
	protected void storeQuery(int image_id, int line,
		Statement st, String data) throws Exception {
		String query = "insert into image_store values(" + image_id
			+ ", " + line + ", '" + data + "')";

		// Print out the query for debug.
		// System.err.println(query);

		st.executeUpdate(query);
	}

	// Takes and image_id, pulls in the image from cache, and goes about
	// encoding it to put it into the database in a transaction.  The last
	// query in the transaction records the image having been stored.
	protected void storeImage(int image_id) throws Exception {
		PhotoImage p = new PhotoImage(image_id);
		SpyDB pdb = getDB();
		Connection db = null;
		Statement st = null;
		Vector v = p.getImage();
		System.err.println("Storer: Got image for " + image_id + " " 
			+ v.size() + " lines of data to store.");
		try {
			int i=0, n=0;
			db = pdb.getConn();
			db.setAutoCommit(false);
			st = db.createStatement();
			BASE64Encoder base64=new BASE64Encoder();
			String data = "";

			for(; i<v.size(); i++) {
				String tmp = base64.encodeBuffer((byte[])v.elementAt(i));
				tmp=tmp.trim();

				if(data.length() < 2048) {
					data+=tmp+"\n";
				} else {
					storeQuery(image_id, n, st, data);
					data=tmp;
					n++;
				}
			}
			// OK, this is sick, but another one right now for the spare.
			if(data.length() > 0) {
				System.err.println("Storer:  Storing spare.");
				storeQuery(image_id, n, st, data);
				n++;
			}
			System.err.println("Storer:  Stored " + n + " lines of data for "
				+ image_id + ".");
			st.executeUpdate("update upload_log set stored=datetime(now())\n"
				+ "\twhere photo_id = " + image_id);
			db.commit();
			// Go ahead and generate a thumbnail.
			p.getThumbnail();
		} catch(Exception e) {
			// If anything happens, roll it back.
			if( st != null) {
				try {
					db.rollback();
				} catch(Exception e3) {
					// Nothing
				}
			}
		} finally {
			if(db!=null) {
				try {
					db.setAutoCommit(true);
				} catch(Exception e) {
					System.err.println("Error:  " + e);
				}
			}
			pdb.freeDBConn();
		}
	}

	// Get a DB connection from the storer pool.
	// We need a different log file to get the thing to work at all.
	protected SpyDB getDB() {
		PhotoConfig conf=new PhotoConfig();
		conf.put("dbcbLogFilePath", "/tmp/storer_db.log");
		SpyDB pdb = new SpyDB(conf);
		return(pdb);
	}

	// Get a list of images that have been added, but not yet added into
	// the database.
	protected void doFlush() {
		SpyDB pdb = getDB();
		Vector v = null;
		try {
			Connection db=pdb.getConn();
			Statement st=db.createStatement();
			String query = "select * from upload_log where stored is null";
			ResultSet rs=st.executeQuery(query);
			v = new Vector();
			while(rs.next()) {
				v.addElement(rs.getString("photo_id"));
			}
		} catch(Exception e) {
			// Do nothing, we'll try again later.
		} finally {
			pdb.freeDBConn();
		}

		// Got the vector, now store the actual images.  This is done so
		// that we don't hold the database connection open whlie we're
		// making the list *and* getting another database connection to act
		// on it.
		if(v != null) {
			try {
				for(int i = 0; i<v.size(); i++) {
					String stmp = (String)v.elementAt(i);
					storeImage(Integer.valueOf(stmp).intValue());
				}
			} catch(Exception e) {
				// Don't care, we'll try again soon.
			}
		}
	}

	public void run() {
		// Do a flush at the beginning, just in case stuff has been
		// building up.
		try {
			doFlush();
		} catch(Exception e1) {
			// Don't care, all these can fail, we'll just keep trying.
		}
		for(;;) {
			try {
				PhotoConfig p = new PhotoConfig();
				int m=Integer.valueOf(p.get("storer_sleep")).intValue();
				// Check every x minutes
				sleep(m * 60 * 1000);
			} catch(Exception e) {
			} finally {
				doFlush();
			}
		}
	}

	// In case it's run as its own little thingy.
	public static void main(String args[]) {
		PhotoStorerThread storer=new PhotoStorerThread();
		storer.run();
	}
}
