// Copyright (c) 1999 Dustin Sallings
//
// $Id: Syslog.java,v 1.3 2002/07/10 04:24:43 dustin Exp $

package net.spy;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.UnknownHostException;

/**
 * Send a message via syslog.
 */

public class Syslog extends Object {
	final static int EMERG=0;
	final static int ALERT=1;
	final static int CRIT=2;
	final static int ERR=3;
	final static int WARNING=4;
	final static int NOTICE=5;
	final static int INFO=6;
	final static int DEBUG=7;

	final static int KERN = 0;
	final static int USER = 8;
	final static int MAIL = 16;
	final static int DAEMON = 24;
	final static int AUTH = 32;
	final static int SYSLOG = 40;
	final static int LPR = 48;
	final static int NEWS = 56;
	final static int UUCP = 64;
	final static int CRON = 72;
	final static int AUTHPRIV = 80;
	final static int FTP = 88;
	final static int LOCAL0 = 128;
	final static int LOCAL1 = 136;
	final static int LOCAL2 = 144;
	final static int LOCAL3 = 152;
	final static int LOCAL4 = 160;
	final static int LOCAL5 = 168;
	final static int LOCAL6 = 176;
	final static int LOCAL7 = 184;

	String loghost=null;
	InetAddress addr=null;

	public Syslog(String loghost) throws UnknownHostException {
		super();
		this.loghost=loghost;
		addr=InetAddress.getByName(loghost);
	}

	public void log(int facility, int level, String msg) {
		int fl=facility | level;

		String what="<" + fl + ">" + msg;
		// System.out.println(what);

		try {
			DatagramPacket dp = new DatagramPacket(what.getBytes(),
				what.length(), addr, 514);
			DatagramSocket s = new DatagramSocket();
			s.send(dp);
		} catch(Exception e) {
			System.err.println("Error sending syslog packet: " + e);
			e.printStackTrace();
		}
	}

	public static void main(String args[]) throws Exception {
		Syslog l = new Syslog("keyhole");

		l.log(AUTH, NOTICE, args[0]);
	}
}
