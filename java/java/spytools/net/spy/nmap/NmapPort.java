// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: NmapPort.java,v 1.1 2000/01/26 21:38:09 dustin Exp $

package net.spy.nmap;

import java.lang.*;
import java.util.*;
import net.spy.*;

public class NmapPort extends Object {

	protected int _port=-1;
	protected String _status=null;
	protected String _proto=null;
	// I don't know what this is
	protected String _huh=null;
	protected String _service=null;
	// I don't know what these are
	protected String _huh2=null;
	protected String _huh3=null;

	/**
	 * Construct an NmapPort object from an array of the stuff found
	 * within.
	 */
	public NmapPort(String input[]) {
		super();
		_port=Integer.parseInt(input[0]);
		_status=input[1];
		_proto=input[2];
		// the other three doesn't show up because it's blank... I hope
		_service=input[3];
	}

	/**
	 * gets the port number
	 */
	public int port() {
		return(_port);
	}

	 /**
	  * gets the status
	  */
	 public String status() {
		return(_status);
	 }

	 /**
	  * gets the protocol (tcp, udp, etc...)
	  */
	 public String proto() {
		return(_proto);
	 }

	 /**
	  * gets the service type (http/imap/smtp/etc...)
	  */
	 public String service() {
		return(_service);
	 }
}
