// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: FileServlet.java,v 1.2 2000/11/06 10:04:51 dustin Exp $

package net.spy.dsservlet;

import java.io.*;
import java.sql.*;
import java.util.*;

import javax.servlet.*;
import javax.servlet.http.*;

import com.oreilly.servlet.MailMessage;

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

		// Get the DSBean out of the session
		DSBean dsbean=(DSBean)session.getValue("dsbean");
		if(dsbean==null) {
			throw new ServletException("No DSBean in session");
		}

		// Get the file ID parameter
		String show_id=request.getParameter("show_id");
		if(show_id==null) {
			throw new ServletException("Show show_id passed in");
		}

		Show show=null;
		try {
			// Get a Show object for this request
			show=new Show(show_id, dsbean.getUsername());
		} catch(Exception e) {
			throw new ServletException("Error getting Show:  " + e);
		}

		log(dsbean.getUsername() + " requested " + show_id);

		// Send the data
		File f=show.getFile();
		response.setContentType("video/quicktime");
		sendFile(f, response.getOutputStream());

		log(dsbean.getUsername() + " received " + show_id);

		complete(show, dsbean);

		// Done!
	}

	// If this thing fails, it'll blow up and we will not mark the item as
	// having been sent.
	protected void sendFile(File f, OutputStream os) throws IOException {
		long len=f.length();
		InputStream is=new FileInputStream(f);
		byte buffer[]=new byte[8192];
		while(len>0) {
			byte tmpbuf[]=null;
			if(len>=8192) {
				tmpbuf=buffer;
			} else {
				tmpbuf=new byte[(int)len];
			}
			int lenread=is.read(tmpbuf);
			if(lenread>0) {
				len-=lenread;
			}
			os.write(tmpbuf);
		}
		try {
			os.close();
		} catch(IOException e) {
			// Oh well...
		}
	}

	protected void complete(Show show, DSBean dsbean) {
		// Mark it as having been sent if we haven't already
		if(!show.isComplete()) {
			try {
				show.markSent();
			} catch(Exception e) {
				// Not a lot we can do at this point, we tried
			}
		}
		// Send mail about it, let us know when a file's been picked up
		try {
			MailMessage mail=new MailMessage("bob");
			mail.from("dustin+daddyshow-errors@spy.net");
			mail.to("dustin+daddyshow@spy.net");
			mail.setSubject("Delivered DaddyShow " + show.getShowID());
			PrintStream outstr=mail.getPrintStream();
			outstr.print("Delivered DaddyShow " + show.getShowID());
			outstr.print(" to " + dsbean.getUsername() + ".");
			mail.sendAndClose();
		} catch(Exception e) {
		}
	}
}
