/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoImage.java,v 1.5 2000/05/01 04:32:33 dustin Exp $
 */

package net.spy.photo;

import java.io.*;
import java.sql.*;
import java.util.*;
import java.rmi.Naming;
import sun.misc.*;

import net.spy.*;
import net.spy.rmi.*;
import net.spy.util.*;

// The class
public class PhotoImage extends PhotoHelper
{ 
	static ImageServer server = null;
	int image_id=-1;

	public PhotoImage(int which) throws Exception {
		super();
		image_id = which;
	}

	public PhotoImage(int which, RHash r) throws Exception {
		super();
		image_id = which;
	}

	public void finalize() throws Throwable {
		super.finalize();
	}

	// Get an Image
	public Vector getImage() throws Exception {
		ImageData data=null;
		ensureConnected();
		log("Getting image " + image_id + " from ImageServer");
		data=server.getImage(image_id, false);
		if(data==null) {
			throw new Exception("Data was null!");
		}
		if(data.image_data == null) {
			throw new Exception("Contents were null!");
		}
		log("Returning image data (" + data.image_data.size() + " lines)");
		return(data.image_data);
	}

	// Store an image
	public void storeImage(ImageData image_data) throws Exception {
		ensureConnected();
		log("Storing image " + image_id);
		server.storeImage(image_id, image_data);
		log("Stored image " + image_id);
	}

	// Get a thumbnail
	public Vector getThumbnail() throws Exception {
		ImageData data=null;
		ensureConnected();
		log("Getting image " + image_id + " (as thumbnail) from ImageServer");
		data=server.getImage(image_id, true);
		return(data.image_data);
	}

	// Make sure we're connected to an image server
	protected void ensureConnected() throws Exception {
		boolean needconn=true;

		try {
			// If ping works, we don't need a new connection
			if(server.ping()) {
				needconn=false;
			}
		} catch(Exception e) {
			// nevermind
		}

		if(needconn) {
			log("Connecting to ImageServer");
			String serverpath=conf.get("imageserver");
			log("Locating " + serverpath);
			server=(ImageServer)Naming.lookup(serverpath);
			if(server==null) {
				throw new Exception("Can't get a server object");
			}
		}
	}
}
