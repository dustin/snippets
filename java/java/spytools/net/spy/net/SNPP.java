// SNPP implementation
//
// Copyright (c) 1999 Dustin Sallings
//
// $Id: SNPP.java,v 1.3 2000/01/24 06:40:41 dustin Exp $

package net.spy.net;

import java.io.*;
import java.net.*;

/**
 * SNPP client.
 */

public class SNPP {
	protected Socket s;
	protected InputStream in;
	protected OutputStream out;
	protected BufferedReader din;
	protected PrintWriter prout;

	/**
	 * Current full line received from the SNPP server.
	 */
	public String currentline;
	/**
	 * Current message received from the SNPP server.
	 */
	public String currentmessage;
	/**
	 * Current status received from SNPP server.
	 */
	public int currentstatus;
	/**
	 * Debug mode on/off.
	 */
	public boolean debug = false;

	/**
	 * Get a new SNPP object connected to host:port
	 *
	 * @param host SNPP host to connect to
	 * @param port SNPP port number
	 *
	 * @exception IOException Thrown if the various input and output
	 * streams cannot be established.
	 *
	 * @exception UnknownHostException Thrown if the SNPP server hostname
	 * cannot be resolved.
	 */
	public SNPP(String host, int port)
		throws IOException, UnknownHostException {
		s = new Socket(host, port);

		in=s.getInputStream();
		din = new BufferedReader(new InputStreamReader(in));
		out=s.getOutputStream();
		prout=new PrintWriter(out);

		getaline();
	}

	/**
	 * Send a simple page.
	 *
	 * @param id SNPP recipient ID.
	 * @param msg msg to send.
	 *
	 * @exception Exception Thrown if any of the commands required to send
	 * the page threw an exception.
	 */
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

	/**
	 * Send an SNPP command.
	 *
	 * @param command command to send.  It's sent literally to the SNPP
	 * server.
	 *
	 * @exception Exception Thrown if the command does not return an ``OK''
	 * status from the SNPP server.
	 */
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

	/**
	 * Object finalization.
	 *
	 * @exception Throwable Thrown if finalization fails.
	 */
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

	// Return whether the current status number is within an OK range.
	protected boolean ok() {
		boolean r = false;
		if(currentstatus < 300 ) {
			if(currentstatus >= 200) {
				r = true;
			}
		}
		return(r);
	}

	// Return a line from the SNPP server.
	protected void getaline() throws IOException {
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
