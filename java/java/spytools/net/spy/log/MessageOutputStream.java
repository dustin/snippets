// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: MessageOutputStream.java,v 1.2 2002/07/10 04:25:46 dustin Exp $

package net.spy.log;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;

import java.net.InetAddress;

/**
 * Send SpyMessages when stuff goes to this OutputStream.
 */
public class MessageOutputStream extends OutputStream {

	private MCastLog mcl=null;

	/**
	 * Get an instance of MessageOutputStream.
	 */
	public MessageOutputStream(MCastLog mcl) {
		super();
		this.mcl=mcl;
	}

	/**
	 * Write a byte.
	 */
	public void write(int b) throws IOException {
		byte ba[]=new byte[1];
		ba[0]=(byte)b;
		write(ba, 0, 0);
	}

	/**
	 * Do the actual writing.
	 */
	public void write(byte b[], int offset, int length) throws IOException {
		// Make a string and get rid of the extra space at the ends.
		// String msg_s=new String(b, offset, length).trim();
		while(length>=offset && ( b[length]=='\r' || b[length]=='\n')) {
			length--;
		}
		String msg_s=new String(b, offset, length);
		if(msg_s.trim().length() > 0 ) {
			SpyMessage msg=new SpyMessage(msg_s);
			mcl.sendMessage(msg);
		}
	}

	/**
	 * Redefine stderr to send messages via this thing.
	 */
	public void setErr() {
		System.setErr(new PrintStream(this));
	}

	/**
	 * Redefine stdout to send messages via this thing.
	 */
	public void setOut() {
		System.setOut(new PrintStream(this));
	}

	/**
	 * Clean up.
	 */
	public void close() throws IOException {
		mcl.close();
		super.close();
	}

	/**
	 * Test.
	 */
	public static void main(String args[]) throws Exception {
		MCastLog mcl=new MCastLog(
			InetAddress.getByName("227.227.227.227"), 3432);
		MessageOutputStream mos=new MessageOutputStream(mcl);
		mos.setErr();
		mos.setOut();
		System.out.println("Testing at " + new java.util.Date());
	}

}

