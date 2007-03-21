// Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
// $Id: ImageServerImpl.java,v 1.3 2000/05/01 04:32:30 dustin Exp $

package net.spy;

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.RMISecurityManager;
import java.rmi.server.UnicastRemoteObject;

import java.util.*;
import java.lang.*;
import java.io.*;
import java.sql.*;

import sun.misc.*;

import net.spy.*;
import net.spy.util.*;

public class ImageServerImpl extends UnicastRemoteObject
	implements ImageServer {

	RHash rhash=null;

	public ImageServerImpl() throws RemoteException {
		super();
		getRhash();
	}

	public ImageData getImage(int image_id, boolean thumbnail)
		throws RemoteException {
		ImageData image_data=null;
		try {
			if(thumbnail) {
				log("fetching thumbnail");
				image_data=fetchThumbnail(image_id);
			} else {
				log("fetching full image");
				image_data=fetchImage(image_id);
			}
		} catch(Exception e) {
			throw new RemoteException("Error fetching image", e);
		}
		return(image_data);
	}

	public void storeImage(int image_id, ImageData image)
		throws RemoteException {
		throw new RemoteException("storeImage Not implemented.");
	}

	protected Vector makeThumbnail(ImageData in, int image_id)
		throws Exception {
		Random r = new Random();
		String part = "/tmp/image." + r.nextInt() + "." + image_id;
		String thumbfilename = part + ".tn.jpg";
		String tmpfilename=part + ".jpg";
		Vector v = null;

		try {
			int i=-1;
			Runtime run = Runtime.getRuntime();
			FileOutputStream f = new FileOutputStream(tmpfilename);
			for(i=0;i<in.image_data.size(); i++) {
				f.write( (byte[])in.image_data.elementAt(i));
			}

			log("Converting image");
			Process p = run.exec("convert -size 100x100 "
				+ tmpfilename + " " + thumbfilename);
			p.waitFor();
			log("done converting image");

			byte b[]=new byte[8192];
			FileInputStream fin = new FileInputStream(thumbfilename);
			int size;

			v=new Vector();

			while( (size=fin.read(b)) >=0 ) {
				byte tmp[] = new byte[size];
				for(i=0; i<size; i++) {
					tmp[i]=b[i];
				}
				v.addElement(tmp);
			}
		} catch(Exception e) {
			throw new Exception("Error making thumbnail:  " + e);
		} finally {
			try {
				File f = new File(tmpfilename);
				f.delete();
				f = new File(thumbfilename);
				f.delete();
			} catch(Exception e2) {
				// No need to do anything, that's just cleanup.
			}
		}
		return(v);
	}

	// Fetch a thumbnail of an image 
	protected ImageData fetchThumbnail(int image_id) throws Exception {
		Vector v=null;
		String key="photo_tn_" + image_id;

		if(rhash!=null) {
			v=(Vector)rhash.get(key);
		}

		if(v==null) {
			// Make a thumbnail out of the fullsize image
			log("Making thumbnail (1)");
			v=makeThumbnail(fetchImage(image_id), image_id);
			if(rhash!=null && v != null) {
				log("Storing thumbnail in rhash");
				rhash.put(key, v);
				log("Done storing object.");
			} else {
				log("Either the rhash or the Vector is null.");
			}
		}

		return(new ImageData(v));
	}

	protected void log(String what) {
		System.err.println(what);
	}

	// Fetch an image
	protected ImageData fetchImage(int image_id) throws Exception {
		String query, key;
		BASE64Decoder base64 = new BASE64Decoder();
		Vector v=null;

		key = "photo_" + image_id;

		if(rhash!=null) {
			v=(Vector)rhash.get(key);
		}

		if(v==null) {
			v = new Vector();
			Connection photo;
			try {
				photo=getDBConn();
			} catch(Exception e) {
				throw new Exception("Can't get database connection: " + e);
			}

			query = "select * from image_store where id = " + image_id +
				" order by line";

			try {
				Statement st = photo.createStatement();
				ResultSet rs = st.executeQuery(query);

				while(rs.next()) {
					byte data[];
					data=base64.decodeBuffer(rs.getString(3));
					v.addElement(data);
				}
				if(rhash!=null) {
					rhash.put(key, v);
				}
			} catch(Exception e) {
				throw new Exception("Problem getting image: " + e);
			} finally {
				freeDBConn(photo);
			}
		}

		return(new ImageData(v));
	}

	protected Connection getDBConn() throws Exception {
		Connection photo;
		String source;

		Class.forName("postgresql.Driver");
		source="jdbc:postgresql://localhost/photo";
		photo = DriverManager.getConnection(source, "nobody", "");
		return(photo);
	}

	protected void freeDBConn(Connection conn) {
	}

	// Get a cache object server
	protected void getRhash() {
		try {
			rhash = new RHash("//localhost/RObjectServer");
		} catch(Exception e) {
			rhash=null;
		}
	}

	public static void main(String args[]) { 

		// Create and install a security manager 
		if (System.getSecurityManager() == null) { 
			System.setSecurityManager(new RMISecurityManager()); 
		} 
		try { 
			ImageServerImpl obj = new ImageServerImpl(); 
			// Bind this object instance to the name "RobjectServer" 
			Naming.rebind("ImageServer", obj); 
			System.out.println("ImageServer bound in registry"); 
		} catch (Exception e) { 
			System.out.println("ImageServer err: " + e.getMessage()); 
			e.printStackTrace(); 
		} 
	} 
}
