// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: SpyRunner.java,v 1.10 2002/07/10 04:24:39 dustin Exp $

package net.spy;

import java.io.File;

import java.lang.reflect.InvocationTargetException;

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
	String classname=null;

	static SpyConfig conf=null;

	public SpyRunner(ThreadGroup tg, String object, String args[]) {
		super(tg, "main");
		this.classname=object;
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

			// Run it!
			SpyUtil.runClass(classname, args);

		} catch(InvocationTargetException ite) {
			System.err.println("Error invoking method for " + classname
				+ ":  " + ite);
			ite.printStackTrace();
			Throwable t = ite.getTargetException();
			System.err.println("Original:  " + t);
			t.printStackTrace();
		} catch(Exception e) {
			System.err.println("Error invoking method for " + classname
				+ ":  " + e);
			e.printStackTrace();
		}
	}

	/**
	 * The main portion of SpyRunner.
	 */
	public static void main(String args[]) throws Exception {
		conf=new SpyConfig(new File(args[0]));
		String apps=conf.get("apps");

		ThreadGroup system=getSystemGroup();

		String a[]=SpyUtil.split(" ", apps);
		for(int i=0; i<a.length; i++) {
			// System.out.println("Got app:  " + a[i]);

			String classname=conf.get(a[i] + ".class");
			String argstring=conf.get(a[i] + ".args");
			String cargs[]=null;
			if(argstring!=null && argstring.length() > 0) {
				// System.out.println("Got an argstring:  " + argstring);
				cargs=SpyUtil.split(" ", argstring);
			} else {
				// System.out.println("No argstring, using an empty one");
				cargs=new String[0];
			}

			ThreadGroup tg=new ThreadGroup(system,  a[i]);

			System.out.println("Starting " + classname);
			Thread t = new SpyRunner(tg, classname, cargs);
			t.setName(a[i]);
			t.start();
			System.out.println("Started...");
			// Wait a second before starting each thing, in case there are
			// dependencies
			try {
				Thread.sleep(1000);
			} catch(Exception e) {
				// OK, maybe not.
			}
		}
		// system.list();
	}

    // Get the system threadgroup for initialize
    private static ThreadGroup getSystemGroup() {
        ThreadGroup start=null, last=null;

        for(start=Thread.currentThread().getThreadGroup(); start!=null;) {
            last=start;
            start=start.getParent();
        }
        return(last);
    }
}
