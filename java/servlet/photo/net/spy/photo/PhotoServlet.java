/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoServlet.java,v 1.10 2000/06/24 08:03:15 dustin Exp $
 */

package net.spy.photo;

import java.io.*;
import java.sql.*;
import java.util.*;
import java.net.*;
import sun.misc.*;

import javax.servlet.*;
import javax.servlet.http.*;

import net.spy.*;

// The class
public class PhotoServlet extends HttpServlet
{ 
	// Only *really* persistent data can go here.
	public String self_uri=null;
	public RHash rhash=null;
	public PhotoSecurity security = null;

	public SpyLog logger = null;

	// Users
	public Hashtable userdb=null;

	// The once only init thingy.
	public void init(ServletConfig config) throws ServletException {
		super.init(config);

		PhotoConfig conf = new PhotoConfig();

		// Populate the userdb hash
		try {
			userdb=new Hashtable();
			log("Initing userdb");
			init_userdb();
			log("Finished userdb");
		} catch(Exception e) {
			log("Error getting user database:  " + e);
			e.printStackTrace();
			throw new ServletException("Couldn't get userdb:  " + e);
		}

		// Security stuff
		try {
			log("Initing security");
			security = new PhotoSecurity();
			security.setUserHash(userdb);
			log("Finished security");
		} catch(Exception e) {
			throw new ServletException("Can't create security stuff:  " + e);
		}

		// Get an rhash to cache images and shite.
		try {
			log("Initing rhash");
			rhash = new RHash(conf.get("objectserver"));
			log("got rhash");
		} catch(Exception e) {
			log("Could not get rhash connection:  " + e);
			rhash = null;
		}

		log("Initing logger");
		logger = new SpyLog(new PhotoLogFlusher());
		log("got logger");
		log("Initialization complete");
	}

	protected void init_userdb() throws Exception {
		Connection photo=null;
		boolean worked=false;
		Exception why=null;
		SpyDB pdb = new SpyDB(new PhotoConfig());

		try {
			photo=pdb.getConn();
			Statement st = photo.createStatement();
			ResultSet rs=st.executeQuery("select * from wwwusers");

			while(rs.next()) {
				PhotoUser u = new PhotoUser();

				u.id=new Integer(rs.getInt("id"));
				u.username=rs.getString("username");
				u.password=rs.getString("password");
				u.email=rs.getString("email");
				u.realname=rs.getString("realname");
				u.canadd=rs.getBoolean("canadd");

				log("Adding user " + u.username);
				userdb.put(u.username, u);
			}
			worked=true;
		} catch(Exception e) {
			worked=false;
			why=e;
		} finally {
			pdb.freeDBConn(photo);
		}

		if(worked==false) {
			throw new ServletException("Couldn't get list:  " + why);
		}
	}

	// Verify we have a valid rhash, if not, reopen it.
	protected void verify_rhash() {
		boolean needy=false;
		if(!rhash.connected()) {
			log("Need a new rhash");
			try {
				// Try to reopen it
				PhotoConfig conf = new PhotoConfig();
				log("Getting rhash from " + conf.get("objectserver"));
				rhash = new RHash(conf.get("objectserver"));
				log("Got a new rhash");
			} catch(Exception e) {
				rhash=null;
				log("Error getting rhash:  " + e);
			}
		}
	}

	// Do a GET request
	public void doGet (
		HttpServletRequest request, HttpServletResponse response
	) throws ServletException, IOException {

		verify_rhash();
		PhotoSession ps = new PhotoSession(this, request, response);
		ps.process();
	}

	// Do a POST request
	public void doPost (
		HttpServletRequest request, HttpServletResponse response
	) throws ServletException, IOException {

		verify_rhash();
		PhotoSession ps = new PhotoSession(this, request, response);
		ps.process();
	}
}
