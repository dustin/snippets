// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: QuickRun.java,v 1.4 2001/02/14 02:14:13 dustin Exp $

package net.spy.util;

import java.util.*;
import java.net.*;
import java.lang.reflect.*;
import java.io.*;

import net.spy.SpyUtil;

/**
 * Listens on a socket and lets you run stuff without firing up a new JVM.
 */

public class QuickRun extends Thread {

	// Server stuff
	private ServerSocket server=null;
	private int port=11011;

	// Client stuff
	private Socket client=null;
	private BufferedReader in=null;
	private PrintWriter out=null;
	private OutputStream ostream=null;

	public QuickRun(Socket s) throws Exception {
		super();
		this.client=s;
	}

	public QuickRun(int port) throws Exception {
		super();
		this.port=port;
	}

	public void run() {
		try {
			in = new BufferedReader(
				new InputStreamReader(client.getInputStream()));
			ostream=client.getOutputStream();
		out = new PrintWriter(new OutputStreamWriter(ostream));

			// Greet the client
			out.println("QuickRun ready.");
			out.flush();

			// Get the command.
			String cmd=in.readLine();

			processCmd(cmd);
		} catch(Exception e) {
			System.err.println("QuickRun exception:  " +e);
			e.printStackTrace();
		} finally {
			try { ostream.close(); } catch(Exception e) {}
			try { out.close(); } catch(Exception e) {}
			try { in.close(); } catch(Exception e) {}
		}
	}

	private void processCmd(String line) throws Exception {
		String cmd=null, args=null;
		int colon=line.indexOf(":");
		cmd=line.substring(0, colon);
		args=line.substring(colon+1);

		System.out.println("Got command:  " + cmd + " args:  " + args);

		if(cmd.equals("RUN")) {
			runClass(SpyUtil.split(" ", args));
		/*
		} else if(cmd.equals("COMPILE")) {
			// Grab the Sun compiler
			sun.tools.javac.Main compiler=
				new sun.tools.javac.Main(ostream, "javac");
			compiler.compile(SpyUtil.split(" ", args));
		*/
		} else if(cmd.equalsIgnoreCase("QUIT")) {
			out.close();
			ostream.close();
			in.close();
		} else {
			out.println("Invalid command:  " + cmd);
		}
	}

	private void runClass(String args[]) throws Exception {
		String cn=args[0];
		String cargs[]=new String[args.length-1];
		// Copy the args over.
		for(int i=0; i<cargs.length; i++) {
			cargs[i]=args[i+1];
		}

		// Find the class
		Class tclass=Class.forName(cn);

		// Find the method
		Class paramtypes[] = new Class[1];
		String tmp[]=new String[0];
		paramtypes[0]=tmp.getClass();
		Method m = tclass.getMethod("main", paramtypes);

		// Set the args
		Object params[]=new Object[1];
		params[0]=cargs;

		// Run it!
		m.invoke(tclass, params);
	}

	public void goServer() throws Exception {
		server=new ServerSocket(port);
		System.out.println("QuickRun server listening on port " + port);

		while(true) {
			Socket client=server.accept();
			client.setSoTimeout(15000);
			QuickRun c=new QuickRun(client);
			c.start();
		}
	}

	public static void main(String args[]) throws Exception {
		QuickRun qr=new QuickRun(Integer.parseInt(args[0]));
		qr.goServer();
	}
}
