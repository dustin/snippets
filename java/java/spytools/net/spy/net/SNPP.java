// SNPP implementation
//
// Copyright (c) 1999 Dustin Sallings
//
// $Id: SNPP.java,v 1.7 2000/01/28 19:14:46 dustin Exp $

package net.spy.net;

import java.io.*;
import java.net.*;

import net.spy.*;

/**
 * SNPP client.
 */

public class SNPP {
	protected Socket s;
	protected InputStream in;
	protected OutputStream out;
	protected BufferedReader din;
	protected PrintWriter prout;

	// 2way support
	protected boolean goes_both_ways=true;
	protected String msg_tag=null;

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
	 * Put this into 2way mode.
	 *
	 * @exception Exception when the 2way command fails
	 */
	public void twoWay() throws Exception {
		cmd("2way");
		goes_both_ways=true;
	}

	/**
	 * sets the pager ID
	 *
	 * @param id snpp pager id
	 *
	 * @exception Exception when the page command fails
	 */
	public void pagerID(String id) throws Exception {
		cmd("page " + id);
	}

	/**
	 * sets the message to send
	 *
	 * @param msg snpp msg
	 *
	 * @exception Exception when the command fails
	 */
	public void message(String msg) throws Exception {
		cmd("mess " + msg);
	}

	/**
	 * gets the message tag on a 2way page
	 *
	 * @return the tag, or null if there is no tag
	 */
	public String getTag() {
		return(msg_tag);
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
		if(goes_both_ways) {
			twoWay();
		}
		pagerID(id);
		message(msg);
		// My pager server supports priority, so we'll ignore any errors
		// with this one.
		try {
			cmd("priority high");
		} catch(Exception e) {
		}
		send();
	}

	/**
	 * send is handled separately in case it's a two-way transaction.
	 *
	 * @exception Exception Thrown if the send command fails.
	 */
	public void send() throws Exception {
		cmd("send");
		if(goes_both_ways) {
			// If it looks 2way, we get the stuff
			if(currentstatus >= 860) {
				String a[]=SpyUtil.split(" ", currentmessage);
				msg_tag=a[0] + " " + a[1];
			}
		}
	}

	/**
	 * Check for a response from a 2way message.
	 *
	 * @param msg_tag the message tag to look up.
	 *
	 * @return the response message, or NULL if it's not ready
	 *
	 * @exception Exception when the msta command fails, or we're not doing
	 * 2way.
	 */
	public String getResponse(String tag) throws Exception {
		String ret=null;
		if(goes_both_ways) {
			cmd("msta " + tag);
			if(currentstatus == 889) {
				String tmp=new String(currentmessage);
				tmp=tmp.substring(tmp.indexOf(" ")).trim();
				tmp=tmp.substring(tmp.indexOf(" ")).trim();
				tmp=tmp.substring(tmp.indexOf(" ")).trim();
				ret=tmp;
			}
		} else {
			throw new Exception("I don't go both ways.");
		}
		return(ret);
	}

	/**
	 * Check for a response from a 2way message.
	 *
	 * @return the response message, or NULL if it's not ready
	 *
	 * @exception Exception when the msta command fails, or we're not doing
	 * 2way.
	 */
	public String getResponse() throws Exception {
		if(msg_tag == null) {
			throw new Exception("No msg tag received, have you done a "
				+ "2way page yet?");
		}
		return(getResponse(msg_tag));
	}

	/**
	 * adds a response to the SNPP message.
	 *
	 * @param response the canned response to add
	 *
	 * @exception Exception when we're not in a 2way transaction, or the
	 * command fails.
	 */
	public void addResponse(String response) throws Exception {
		if(!goes_both_ways) {
			throw new Exception("I don't go both ways.");
		}
		cmd("mcre " + response);
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

	protected void finalize() throws Throwable {
		if(debug) {
			System.out.println("Finalizing...");
		}
		try {
			cmd("quit");
			s.close();
		} catch(Exception e) {
		}
		s=null;
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
		// Specific stuff for two-way
		if(goes_both_ways && r == false) {
			if(currentstatus < 890 && currentstatus >= 860) {
				// delivered, processing or final
				r=true;
			} else if(currentstatus < 970 && currentstatus >= 960) {
				// Queued transaction
				r=true;
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
