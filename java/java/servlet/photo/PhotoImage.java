/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoImage.java,v 1.1 1999/09/28 04:47:01 dustin Exp $
 */

import java.io.*;
import java.sql.*;
import java.text.*;
import java.util.*;
import java.net.*;
import sun.misc.*;

// import javax.servlet.*;
// import javax.servlet.http.*;

import com.oreilly.servlet.*;
import com.javaexchange.dbConnectionBroker.*;


// The class
public class PhotoImage
{
	DbConnectionBroker dbs;
	RHash rhash;
	Vector image_data;

	private void getRhash() {
		// Get an rhash to cache images and shite.
		try {
			rhash = new RHash("//dhcp-104/RObjectServer");
		} catch(Exception e) {
			rhash = null;
		}
	}

	private void getDBS() throws Exception {
		try {
			Class.forName("postgresql.Driver");
			String source="jdbc:postgresql://dhcp-104/photo";
			dbs = new DbConnectionBroker("postgresql.Driver",
				source, "dustin", "", 2, 5, "/tmp/pool.log", 0.01);
		} catch(Exception e) {
			// System.err.println("dbs broke:  " + e.getMessage());
			throw new Exception ("dbs broke: " + e.getMessage());
		}
	}

	public PhotoImage() throws Exception {
		super();
		image_data = null;
		getRhash();
		getDBS();
	}

	public PhotoImage(DbConnectionBroker db, RHash r) {
		super();
		image_data = null;
		dbs=db;
		rhash=r;
	}

	private void log(String message) {
		System.err.println("PhotoImage: " + message);
	}

	// Grab a connection from the pool.
	private Connection getDBConn() throws SQLException {
		Connection photo;

		// The path to the database...
		photo = dbs.getConnection();
		return(photo);
	}

	// Gotta free the connection
	private void freeDBConn(Connection conn) {
		dbs.freeConnection(conn);
	}

	// Show an image
	public Vector fetchImage(int id) throws Exception {

		String query, key;
		BASE64Decoder base64 = new BASE64Decoder();
		image_data = new Vector();

		image_data = null;

		key = "photo_" + id;

		if(rhash!=null) {
			image_data = (Vector)rhash.get(key);
		} else {
			log("No rhash for image cache, must use database directly");
		}

		if(image_data==null) {

			image_data = new Vector();
			Connection photo;
			try {
				photo=getDBConn();
			} catch(Exception e) {
				throw new Exception("Can't get database connection: "
					+ e.getMessage());
			}
			query = "select * from image_store where id = " + id +
				" order by line";

			System.out.print("Doing query:  " + query + "\n");

			try {

				Statement st = photo.createStatement();
				ResultSet rs = st.executeQuery(query);

				log("Getting image " + id + " from database.");

				while(rs.next()) {
					byte data[];
					data=base64.decodeBuffer(rs.getString(3));
					image_data.addElement(data);
				}
				if(rhash != null) {
					log("Storing " + key + " in RHash");
					rhash.put(key, image_data);
				} else {
					log("No RHash, can't cache data.");
				}
			} catch(Exception e) {
				throw new Exception("Problem getting image: " +
					e.getMessage());
			}
			finally { freeDBConn(photo); }
		} else {
			log("Got image " + id + " from RHash.");
		}

		return(image_data);
	}
}
