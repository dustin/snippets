/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoImage.java,v 1.5 1999/10/04 06:32:19 dustin Exp $
 */

import java.io.*;
import java.sql.*;
import java.util.*;
import sun.misc.*;

import com.javaexchange.dbConnectionBroker.*;

// The class
public class PhotoImage extends PhotoHelper
{
	RHash rhash=null;
	PhotoImageData image_data=null;
	boolean wascached=false;
	String tmpfilename, thumbfilename=null;
	int image_id;

	private void getRhash() {
		// Get an rhash to cache images and shite.
		try {
			rhash = new RHash("//dhcp-104/RObjectServer");
		} catch(Exception e) {
			rhash = null;
		}
	}

	public PhotoImage(int which) throws Exception {
		super();
		image_id = which;
		getRhash();
	}

	public PhotoImage(int which, DbConnectionBroker db, RHash r) {
		super(db);
		image_id = which;
		rhash=r;
	}

	// Find out if the last fetch was cached.
	public boolean wasCached() {
		return(wascached);
	}

	protected void makeThumbNail() throws Exception {
		if(image_data == null || image_data.image_data == null) {
			throw new Exception("Need to fetchImage first");
		}
		Random r = new Random();
		String part = "/tmp/image." + r.nextInt() + "." + image_id;

		thumbfilename = part + ".tn.jpg";
		tmpfilename=part + ".jpg";

		log("Filename is " + tmpfilename);

		try {
			int i;
			Runtime run = Runtime.getRuntime();

			FileOutputStream f = new FileOutputStream(tmpfilename);
			for(i=0;i<image_data.image_data.size(); i++) {
				f.write( (byte[])image_data.image_data.elementAt(i));
			}

			log("Converting.");
			Process p = run.exec("convert -size 100x100 "
				+ tmpfilename + " " + thumbfilename);
			p.waitFor();
			log("Done converting.");

			byte b[]=new byte[1024];
			FileInputStream fin = new FileInputStream(thumbfilename);
			int size;

			image_data.thumbnail_data = new Vector();

			while( (size=fin.read(b)) >=0 ) {
				byte tmp[] = new byte[size];

				for(i=0; i<size; i++) {
					tmp[i]=b[i];
				}
				image_data.thumbnail_data.addElement(tmp);
			}

		} catch(IOException e) {
			throw new Exception("IO Exception:  " + e);
		} finally {
			deleteTempFiles();
		}
	}

	// Delete the tmpfile.
	protected void deleteTempFiles() {
		try {
			if(tmpfilename!=null) {
				File f = new File(tmpfilename);
				f.delete();
				f = new File(thumbfilename);
				f.delete();
			}
		} catch(Exception e) {
		}
	}

	public void finalize() throws Throwable {
		super.finalize();
	}

	public Vector getImage() throws Exception {
		if(image_data==null) {
			fetchImage();
		}
		return(image_data.image_data);
	}

	// Make a thumbnail
	public Vector getThumbnail() throws Exception {

		if(image_data==null) {
			image_data = new PhotoImageData();
		}

		String key= "photo_tn_" + image_id;

		if(rhash!=null) {
			image_data.thumbnail_data = (Vector)rhash.get(key);
		}

		// Did we get it?
		if(image_data.thumbnail_data==null) {
			if(image_data.image_data==null) {
				fetchImage();
			}
			makeThumbNail();
			rhash.put(key, image_data.thumbnail_data);
		}

		return(image_data.thumbnail_data);
	}

	// Show an image
	protected void fetchImage() throws Exception {

		String query, key;
		BASE64Decoder base64 = new BASE64Decoder();

		if(image_data.image_data!=null) {
			return;
		}

		image_data = new PhotoImageData();

		key = "photo_" + image_id;

		if(rhash!=null) {
			image_data.image_data = (Vector)rhash.get(key);
			wascached=true;
		} else {
			log("No rhash for image cache, must use database directly");
			wascached=false;
		}

		if(image_data.image_data==null) {

			image_data.image_data = new Vector();
			Connection photo;
			try {
				photo=getDBConn();
			} catch(Exception e) {
				throw new Exception("Can't get database connection: "
					+ e.getMessage());
			}
			query = "select * from image_store where id = " + image_id +
				" order by line";

			// System.out.print("Doing query:  " + query + "\n");

			try {

				Statement st = photo.createStatement();
				ResultSet rs = st.executeQuery(query);

				log("Getting image " + image_id + " from database.");
				wascached=false;

				while(rs.next()) {
					byte data[];
					data=base64.decodeBuffer(rs.getString(3));
					image_data.image_data.addElement(data);
				}
				if(rhash != null) {
					log("Storing " + key + " in RHash");
					try {
						makeThumbNail();
					} catch(Exception e2) {
						e2.printStackTrace();
					}
					rhash.put(key, image_data.image_data);
				} else {
					log("No RHash, can't cache data.");
				}
			} catch(Exception e) {
				throw new Exception("Problem getting image: " +
					e.getMessage());
			}
			finally { freeDBConn(photo); }

		} else {
			log("Got image " + image_id + " from RHash.");
		}
	}
}
