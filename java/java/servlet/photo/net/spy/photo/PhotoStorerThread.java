/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoStorerThread.java,v 1.3 1999/11/26 05:28:27 dustin Exp $
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

	// Takes and image_id, pulls in the image from cache, and goes about
	// encoding it to put it into the database in a transaction.  The last
	// query in the transaction records the image having been stored.
	protected void storeImage(int image_id) throws Exception {
		PhotoImage p = new PhotoImage(image_id);
		SpyDB pdb = new SpyDB(new PhotoConfig());
		Connection db = null;
		Statement st = null;
		Vector v = p.getImage();
		System.err.println("Got image for " + image_id );
		try {
			db = pdb.getConn();
			db.setAutoCommit(false);
			st = db.createStatement();
			BASE64Encoder base64=new BASE64Encoder();

			for(int i = 0; i<v.size(); i++) {
				String tmp = base64.encodeBuffer((byte[])v.elementAt(i));
				String query = "insert into image_store values(" + image_id
					+ ", " + i + ", '" + tmp + "')";
				st.executeUpdate(query);
			}
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

	// Get a list of images that have been added, but not yet added into
	// the database.
	protected void doFlush() {
		SpyDB pdb = new SpyDB(new PhotoConfig());
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
}
