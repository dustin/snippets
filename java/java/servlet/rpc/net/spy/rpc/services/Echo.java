// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Echo.java,v 1.1 2002/11/11 18:22:19 dustin Exp $

package net.spy.rpc.services;

import java.util.Vector;
import java.util.Hashtable;
import java.util.Date;

import org.apache.xmlrpc.XmlRpcException;

/**
 * XMLRPC testing method.
 */
public class Echo extends Remote {

	/**
	 * Get an instance of Temperatures.
	 */
	public Echo() {
		super();
	}

	/**
	 * Echo a string.
	 */
	public String echoString(String msg) {
		return(msg);
	}

	/**
	 * Throw an exception.
	 */
	public String fault() throws XmlRpcException {
		throw new XmlRpcException(77, "You asked for it.");
	}

	/**
	 * Echo a date.
	 */
	public Date echoDate(Date d) {
		return(d);
	}

	/**
	 * Echo an integer.
	 */
	public int echoInt(int i) {
		return(i);
	}

	/**
	 * Echo a boolean.
	 */
	public boolean echoBoolean(boolean b) {
		return(b);
	}

	/**
	 * Echo a double.
	 */
	public double echoDouble(double d) {
		return(d);
	}

	/**
	 * Echo a chunk of binary data (base64 test).
	 */
	public byte[] echoByte(byte data[]) {
		return(data);
	}

	/**
	 * Echo an array.
	 */
	public Vector echoArray(Vector in) {
		return(in);
	}

	/**
	 * Echo a struct.
	 */
	public Hashtable echoStruct(Hashtable h) {
		return(h);
	}

}
