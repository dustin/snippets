// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: SpyRunner.java,v 1.3 2000/01/24 06:40:36 dustin Exp $

package net.spy;

import java.lang.*;
import java.lang.reflect.*;
import java.util.*;
import java.io.*;

/**
 * SpyRunner - a hack-ass application server.
 * <p>
 * This is a stanalone application that will run other standalone
 * applications in its process space.
 * <p>
 * Configuration for what to start is passed in as the first argument upon
 * execution, and comes in the form of a properties file.  An example
 * properties file is as follows:
 * <p>
 * <pre>
 * # List of applications to start.
 * apps	app1 app2
 *
 * # Configuration for app1
 * app1.class	com.example.app1
 * app1.args	Arguments for app1
 *
 * # Configuration for app2
 * app2.class   com.example.app2
 * app2.args    Arguments for app2
 * </pre>
 */

public class SpyRunner extends Thread {
	Class tclass=null;
	String args[]=null;

	static Properties prop=null;

	public SpyRunner(String object, String args[]) {
		try {
			tclass=Class.forName(object);
			this.args=args;
		} catch(Exception e) {
			System.err.println("Error loading class " + object + ":  " + e);
			e.printStackTrace();
		}
	}

	public void run() {
		try {
			// Class list to find the method dynamically
			Class paramtypes[] = new Class[1];
			String tmp[]=new String[0];
			paramtypes[0]=tmp.getClass();
			Method m = tclass.getMethod("main", paramtypes);

			// Object list to invoke the method
			Object params[]=new Object[1];
			params[0]=args;

			// invoke the method
			m.invoke(tclass, params);
		} catch(Exception e) {
			System.err.println("Error invoking method:  " + e);
			e.printStackTrace();
		}
	}

	public static void main(String args[]) throws Exception {
		initProperties(args[0]);

		String a[]=SpyUtil.split(" ", prop.getProperty("apps"));
		for(int i=0; i<a.length; i++) {
			// System.out.println("Got app:  " + a[i]);

			String classname=prop.getProperty(a[i] + ".class");
			String argstring=prop.getProperty(a[i] + ".args");
			String cargs[]=null;
			if(argstring!=null && argstring.length() > 0) {
				// System.out.println("Got an argstring:  " + argstring);
				cargs=SpyUtil.split(" ", argstring);
			} else {
				// System.out.println("No argstring, using an empty one");
				cargs=new String[0];
			}
			Thread t = new SpyRunner(classname, cargs);
			t.run();
		}
	}

	protected static void initProperties(String from) throws Exception {
		prop=new Properties();
		prop.load(new FileInputStream(from));
	}
}
