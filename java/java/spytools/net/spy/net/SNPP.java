// SNPP implementation
//
// Copyright (c) 1999 Dustin Sallings
//
// $Id: SNPP.java,v 1.2 2000/01/19 08:03:42 dustin Exp $

package net.spy.net;

import java.io.*;
import java.net.*;

public class SNPP {
	protected Socket s;
	protected InputStream in;
	protected OutputStream out;
	protected BufferedReader din;
	protected PrintWriter prout;

	public String currentline;
	public String currentmessage;
	public int currentstatus;
	public boolean debug = false;

	public SNPP(String host, int port)
		throws IOException, UnknownHostException {
		s = new Socket(host, port);

		in=s.getInputStream();
		din = new BufferedReader(new InputStreamReader(in));
		out=s.getOutputStream();
		prout=new PrintWriter(out);

		getaline();
	}

	public void finalize() throws Throwable {
		if(debug) {
			System.out.println("Finalizing...");
		}
		try {
			cmd("quit");
		} catch(Exception e) {
		}
		super.finalize();
	}

	// Send a page, all bottled up and stuff.
	public void sendpage(String id, String msg) throws Exception {
		// Reset so this thing can be called more than once.
		cmd("rese");
		cmd("page " + id);
		cmd("mess " + msg);
		// My pager server supports priority, so we'll ignore any errors
		// with this one.
		try {
			cmd("priority high");
		} catch(Exception e) {
		}
		cmd("send");
	}

	// Return whether the current status number is within an OK range.
	public boolean ok() {
		boolean r = false;
		if(currentstatus < 300 ) {
			if(currentstatus >= 200) {
				r = true;
			}
		}
		return(r);
	}

	// Send an SNPP command.
	public void cmd(String command) throws Exception {
		if(debug) {
			System.out.println(">> " + command);
		}
		prout.print(command + "\r\n");
		prout.flush();
		getaline();
		if(!ok()) {
			throw new Exception(currentmessage);
		}
	}

	// Return a line from the SNPP server.
	private void getaline() throws IOException {
		String stmp;
		Integer itmp;

		// Get the line
		currentline = din.readLine();

		if(debug) {
			System.out.println("<< " + currentline);
		}

		// Extract the message
		currentmessage = currentline.substring(4);

		// Calculate the status number
		stmp = currentline.substring(0, 3);
		itmp = Integer.valueOf(stmp);
		currentstatus = itmp.intValue();
	}
}
