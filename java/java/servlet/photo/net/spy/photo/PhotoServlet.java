/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoServlet.java,v 1.2 1999/12/15 04:18:55 dustin Exp $
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

	protected PhotoStorerThread storer_thread = null;

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
			init_userdb();
		} catch(Exception e) {
			throw new ServletException("Can't get userdb:  " + e);
		}

		// Security stuff
		try {
			String secret = PhotoSecurity.generateSecret();
			security = new PhotoSecurity(secret);
			security.setUserHash(userdb);
		} catch(Exception e) {
			throw new ServletException("Can't create security stuff:  " + e);
		}

		// Get an rhash to cache images and shite.
		try {
			rhash = new RHash(conf.get("objectserver"));
		} catch(Exception e) {
			log("Could not get rhash connection:  " + e);
			rhash = null;
		}

		try {
			storer_thread = new PhotoStorerThread();
			storer_thread.start();
		} catch(Exception e) {
			throw new ServletException("Can't get storer thread");
		}

		logger = new SpyLog(new PhotoLogFlusher());
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

	// Do a GET request
	public void doGet (
		HttpServletRequest request, HttpServletResponse response
	) throws ServletException, IOException {

		PhotoSession ps = new PhotoSession(this, request, response);
		ps.process();
	}

	// Do a POST request
	public void doPost (
		HttpServletRequest request, HttpServletResponse response
	) throws ServletException, IOException {

		PhotoSession ps = new PhotoSession(this, request, response);
		ps.process();
	}
}
