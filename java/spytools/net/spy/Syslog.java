// Copyright (c) 1999 Dustin Sallings
//
// $Id: Syslog.java,v 1.4 2002/07/10 05:41:09 dustin Exp $

package net.spy;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.UnknownHostException;

/**
 * Send a message via syslog.
 */
public class Syslog extends Object {

	public static final int EMERG=0;
	public static final int ALERT=1;
	public static final int CRIT=2;
	public static final int ERR=3;
	public static final int WARNING=4;
	public static final int NOTICE=5;
	public static final int INFO=6;
	public static final int DEBUG=7;

	public static final int KERN = 0;
	public static final int USER = 8;
	public static final int MAIL = 16;
	public static final int DAEMON = 24;
	public static final int AUTH = 32;
	public static final int SYSLOG = 40;
	public static final int LPR = 48;
	public static final int NEWS = 56;
	public static final int UUCP = 64;
	public static final int CRON = 72;
	public static final int AUTHPRIV = 80;
	public static final int FTP = 88;
	public static final int LOCAL0 = 128;
	public static final int LOCAL1 = 136;
	public static final int LOCAL2 = 144;
	public static final int LOCAL3 = 152;
	public static final int LOCAL4 = 160;
	public static final int LOCAL5 = 168;
	public static final int LOCAL6 = 176;
	public static final int LOCAL7 = 184;

	private String loghost=null;
	private InetAddress addr=null;

	/**
	 * Log to a particular log host.
	 */
	public Syslog(String loghost) throws UnknownHostException {
		super();
		this.loghost=loghost;
		addr=InetAddress.getByName(loghost);
	}

	/**
	 * Send a log message.
	 */
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

	/**
	 * Testing.
	 */
	public static void main(String args[]) throws Exception {
		Syslog l = new Syslog("butterfly");

		l.log(AUTH, NOTICE, args[0]);
	}
}
