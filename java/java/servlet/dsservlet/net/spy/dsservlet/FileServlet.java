// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: FileServlet.java,v 1.1 2000/11/06 08:42:50 dustin Exp $

package net.spy.dsservlet;

import java.io.*;
import java.sql.*;
import java.util.*;

import javax.servlet.*;
import javax.servlet.http.*;

import net.spy.*;

public class FileServlet extends HttpServlet {

	public void doGet(
		HttpServletRequest request, HttpServletResponse response
		) throws ServletException, IOException {

		// Get the session
		HttpSession session=request.getSession(false);
		if(session==null) {
			throw new ServletException("No session, can't be valid");
		}

		// Get the username out of the session
		String username=(String)session.getValue("dsusername");
		if(username==null) {
			throw new ServletException("No username in session");
		}

		// Get the file ID parameter
		String show_id=request.getParameter("show_id");
		if(show_id==null) {
			throw new ServletException("Show show_id passed in");
		}

		Show show=null;
		try {
			// Get a Show object for this request
			show=new Show(show_id, username);
		} catch(Exception e) {
			throw new ServletException("Error getting Show:  " + e);
		}

		log(username + " requested " + show_id);

		// Send the data
		File f=show.getFile();
		long len=f.length();
		InputStream is=new FileInputStream(f);
		OutputStream os=response.getOutputStream();
		byte buffer[]=new byte[8192];

		while(len>0) {
			byte tmpbuf[]=null;
			if(len>=8192) {
				tmpbuf=buffer;
			} else {
				tmpbuf=new byte[(int)len];
			}
			int lenread=is.read(tmpbuf);
			if(len>0) {
				len-=lenread;
			}

			os.write(tmpbuf);
		}

		log(username + " received " + show_id);

		// Mark it as having been sent
		try {
			show.markSent();
		} catch(Exception e) {
			// Not a lot we can do at this point...
		}
	}
}
