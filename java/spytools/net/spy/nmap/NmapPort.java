// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: NmapPort.java,v 1.4 2002/07/10 05:41:55 dustin Exp $

package net.spy.nmap;


public class NmapPort extends Object {

	private int port=-1;
	private String status=null;
	private String proto=null;
	// I don't know what this is
	// private String huh=null;
	private String service=null;
	// I don't know what these are
	// private String _huh2=null;
	// private String _huh3=null;

	/**
	 * Construct an NmapPort object from an array of the stuff found
	 * within.
	 */
	public NmapPort(String input[]) {
		super();
		port=Integer.parseInt(input[0]);
		status=input[1];
		proto=input[2];
		// the other three doesn't show up because it's blank... I hope
		service=input[3];
	}

	/**
	 * gets the port number
	 */
	public int port() {
		return(port);
	}

	 /**
	  * gets the status
	  */
	 public String status() {
		return(status);
	 }

	 /**
	  * gets the protocol (tcp, udp, etc...)
	  */
	 public String proto() {
		return(proto);
	 }

	 /**
	  * gets the service type (http/imap/smtp/etc...)
	  */
	 public String service() {
		return(service);
	 }
}
